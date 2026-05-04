import math
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Set

from scheduler.abstract.abstract_node import AbstractNode
from scheduler.core.action import Action
from scheduler.core.mailbox import Mailbox
from scheduler.core.node_response import NodeResponse
from scheduler.settings.network_settings import settings


@dataclass
class TreeBuildState:
    discovered: bool = False
    parent: Optional[uuid.UUID] = None
    pending_neighbors: Set[uuid.UUID] = field(default_factory=set)
    children: Set[uuid.UUID] = field(default_factory=set)
    completed: bool = False


@dataclass
class RoundState:
    round_number: int
    parent_at_start: Optional[uuid.UUID]
    best_distance: float
    best_parent: Optional[uuid.UUID]
    received_from: Set[uuid.UUID] = field(default_factory=set)
    down_sent: bool = False
    improved: bool = False
    completed: bool = False


class Node(AbstractNode):
    START = "START"
    TREE_DISCOVER = "TREE_DISCOVER"
    TREE_ACK = "TREE_ACK"
    ROUND = "ROUND"
    FINAL = "FINAL"

    PHASE_DOWN = "down"
    PHASE_UP = "up"

    def __init__(
        self,
        node_id: uuid.UUID,
        weights: Dict[uuid.UUID, int],
        all_node_ids: List[uuid.UUID],
        initiator_id: uuid.UUID,
    ):
        self.node_id = node_id
        self.mailbox = Mailbox()
        self.weights = dict(weights)
        self.neighbors = sorted(weights.keys(), key=lambda neighbor_id: neighbor_id.int)
        self.all_node_ids = sorted(all_node_ids, key=lambda current_id: current_id.int)
        self.initiator_id = initiator_id
        self.round_limit = len(self.all_node_ids) - 1

        self.output_path = Path(settings.OUTPUT_EVENTS_PATH)
        self.summary_path = Path(settings.OUTPUT_SUMMARY_PATH)

        self.transaction_id: Optional[uuid.UUID] = None
        self.tree_state = TreeBuildState()
        self.parent: Optional[uuid.UUID] = None
        self.initial_tree_parent: Optional[uuid.UUID] = None
        self.dist: float = math.inf
        self.active_round: Optional[RoundState] = None
        self.completed = False
        self.summary_written = False

    def process_action(self, message: Action) -> NodeResponse:
        if self.completed and message.data.get("message_type") != self.FINAL:
            return NodeResponse([])

        message_type = message.data.get("message_type")
        if message_type == self.START:
            actions = self.__handle_start(message.data)
        elif message_type == self.TREE_DISCOVER:
            actions = self.__handle_tree_discover(message.data)
        elif message_type == self.TREE_ACK:
            actions = self.__handle_tree_ack(message.data)
        elif message_type == self.ROUND:
            actions = self.__handle_round(message.data)
        elif message_type == self.FINAL:
            actions = self.__handle_final(message.data)
        else:
            raise ValueError(f"Unsupported message type: {message_type}")
        return NodeResponse(actions)

    def is_completed(self) -> bool:
        return self.completed

    def summary_data(self) -> dict[str, Any]:
        return {
            "node": self.__label(self.node_id),
            "distance": None if math.isinf(self.dist) else int(self.dist),
            "parent": self.__label(self.parent),
            "initial_tree_parent": self.__label(self.initial_tree_parent),
        }

    def __handle_start(self, message_data: dict[str, Any]) -> List[Action]:
        if self.tree_state.discovered:
            return []

        self.transaction_id = message_data["transaction_id"]
        self.tree_state.discovered = True
        self.tree_state.pending_neighbors = set(self.neighbors)
        self.dist = 0
        self.parent = None
        self.initial_tree_parent = None

        self.__log(
            "initiates tree construction and sets dist=0"
        )

        if not self.neighbors:
            return self.__broadcast_final(final_round=0)

        return [
            self.__create_action(
                receiver_id=neighbor_id,
                message_type=self.TREE_DISCOVER,
                transaction_id=self.transaction_id,
            )
            for neighbor_id in self.neighbors
        ]

    def __handle_tree_discover(self, message_data: dict[str, Any]) -> List[Action]:
        sender_id = message_data["sender_id"]
        transaction_id = message_data["transaction_id"]
        if self.transaction_id is None:
            self.transaction_id = transaction_id

        if self.tree_state.discovered:
            self.__log(
                f"rejects duplicate tree discover from {self.__label(sender_id)}"
            )
            return [
                self.__create_action(
                    receiver_id=sender_id,
                    message_type=self.TREE_ACK,
                    transaction_id=transaction_id,
                    accepted=False,
                )
            ]

        self.tree_state.discovered = True
        self.tree_state.parent = sender_id
        self.tree_state.pending_neighbors = set(self.neighbors) - {sender_id}
        self.parent = sender_id
        self.initial_tree_parent = sender_id

        self.__log(
            f"discovers tree parent {self.__label(sender_id)} and forwards to "
            f"{[self.__label(neighbor_id) for neighbor_id in sorted(self.tree_state.pending_neighbors, key=lambda node_id: node_id.int)]}"
        )

        if not self.tree_state.pending_neighbors:
            self.tree_state.completed = True
            return [
                self.__create_action(
                    receiver_id=sender_id,
                    message_type=self.TREE_ACK,
                    transaction_id=transaction_id,
                    accepted=True,
                )
            ]

        return [
            self.__create_action(
                receiver_id=neighbor_id,
                message_type=self.TREE_DISCOVER,
                transaction_id=transaction_id,
            )
            for neighbor_id in sorted(self.tree_state.pending_neighbors, key=lambda node_id: node_id.int)
        ]

    def __handle_tree_ack(self, message_data: dict[str, Any]) -> List[Action]:
        sender_id = message_data["sender_id"]
        accepted = bool(message_data["accepted"])

        self.tree_state.pending_neighbors.discard(sender_id)
        if accepted:
            self.tree_state.children.add(sender_id)

        if self.tree_state.pending_neighbors:
            self.__log(
                f"waits for tree acknowledgements from "
                f"{[self.__label(neighbor_id) for neighbor_id in sorted(self.tree_state.pending_neighbors, key=lambda node_id: node_id.int)]}"
            )
            return []

        if self.tree_state.completed:
            return []

        self.tree_state.completed = True
        if self.node_id == self.initiator_id:
            self.__log("initial spanning tree built; starts Merlin-Segal rounds")
            return self.__start_round(1)

        self.__log(
            f"subtree completed, confirms to parent {self.__label(self.tree_state.parent)}"
        )
        return [
            self.__create_action(
                receiver_id=self.tree_state.parent,
                message_type=self.TREE_ACK,
                transaction_id=self.transaction_id,
                accepted=True,
            )
        ]

    def __handle_round(self, message_data: dict[str, Any]) -> List[Action]:
        round_number = int(message_data["round_number"])
        sender_id = message_data["sender_id"]
        phase = message_data["phase"]
        sender_distance = float(message_data["distance"])

        state = self.__ensure_round_state(round_number)
        if sender_id in state.received_from:
            return []

        state.received_from.add(sender_id)
        candidate_distance = sender_distance + self.weights[sender_id]
        if candidate_distance < state.best_distance:
            state.best_distance = candidate_distance
            state.best_parent = sender_id
            state.improved = True
            self.__log(
                f"round={round_number}: improves distance via {self.__label(sender_id)} "
                f"to {self.__format_distance(state.best_distance)}"
            )
        else:
            self.__log(
                f"round={round_number}: receives {phase} from {self.__label(sender_id)} "
                f"with candidate {self.__format_distance(candidate_distance)}"
            )

        actions: List[Action] = []
        if sender_id == state.parent_at_start and phase == self.PHASE_DOWN and not state.down_sent:
            state.down_sent = True
            actions.extend(self.__send_round_down(state))

        if self.__round_complete(state):
            actions.extend(self.__finish_round(state))
        return actions

    def __handle_final(self, message_data: dict[str, Any]) -> List[Action]:
        if self.completed:
            return []

        self.completed = True
        final_round = int(message_data["final_round"])
        sender_id = message_data["sender_id"]
        self.__log(
            f"accepts final routing announcement for round {final_round} from {self.__label(sender_id)}"
        )
        self.__write_summary(final_round)

        return [
            self.__create_action(
                receiver_id=neighbor_id,
                message_type=self.FINAL,
                transaction_id=self.transaction_id,
                final_round=final_round,
            )
            for neighbor_id in self.neighbors
            if neighbor_id != sender_id
        ]

    def __start_round(self, round_number: int) -> List[Action]:
        self.active_round = RoundState(
            round_number=round_number,
            parent_at_start=None,
            best_distance=0,
            best_parent=None,
            down_sent=True,
        )
        self.dist = 0
        self.__log(f"starts round {round_number} with dist=0")

        if not self.neighbors:
            if round_number >= self.round_limit:
                return self.__broadcast_final(round_number)
            return self.__start_round(round_number + 1)

        return [
            self.__create_action(
                receiver_id=neighbor_id,
                message_type=self.ROUND,
                transaction_id=self.transaction_id,
                round_number=round_number,
                phase=self.PHASE_DOWN,
                distance=self.dist,
            )
            for neighbor_id in self.neighbors
        ]

    def __ensure_round_state(self, round_number: int) -> RoundState:
        if self.active_round is None or self.active_round.round_number != round_number:
            self.active_round = RoundState(
                round_number=round_number,
                parent_at_start=self.parent,
                best_distance=self.dist,
                best_parent=self.parent,
            )
            self.__log(
                f"joins round {round_number} with current parent {self.__label(self.parent)} "
                f"and dist={self.__format_distance(self.dist)}"
            )
        return self.active_round

    def __send_round_down(self, state: RoundState) -> List[Action]:
        receivers = [
            neighbor_id
            for neighbor_id in self.neighbors
            if neighbor_id != state.parent_at_start
        ]
        self.__log(
            f"round={state.round_number}: propagates distance {self.__format_distance(state.best_distance)} "
            f"to {[self.__label(neighbor_id) for neighbor_id in receivers]}"
        )
        return [
            self.__create_action(
                receiver_id=neighbor_id,
                message_type=self.ROUND,
                transaction_id=self.transaction_id,
                round_number=state.round_number,
                phase=self.PHASE_DOWN,
                distance=state.best_distance,
            )
            for neighbor_id in receivers
        ]

    def __round_complete(self, state: RoundState) -> bool:
        return (
            not state.completed
            and state.received_from == set(self.neighbors)
            and (state.parent_at_start is None or state.down_sent)
        )

    def __finish_round(self, state: RoundState) -> List[Action]:
        state.completed = True
        previous_dist = self.dist
        previous_parent = self.parent
        next_dist = state.best_distance
        next_parent = state.best_parent if state.improved else self.parent

        self.dist = next_dist
        if self.node_id != self.initiator_id:
            self.parent = next_parent

        self.__log(
            f"finishes round {state.round_number}: dist "
            f"{self.__format_distance(previous_dist)} -> {self.__format_distance(self.dist)}, "
            f"parent {self.__label(previous_parent)} -> {self.__label(self.parent)}"
        )

        if self.node_id == self.initiator_id:
            self.active_round = None
            if state.round_number >= self.round_limit:
                return self.__broadcast_final(state.round_number)
            return self.__start_round(state.round_number + 1)

        self.active_round = None
        return [
            self.__create_action(
                receiver_id=state.parent_at_start,
                message_type=self.ROUND,
                transaction_id=self.transaction_id,
                round_number=state.round_number,
                phase=self.PHASE_UP,
                distance=self.dist,
            )
        ]

    def __broadcast_final(self, final_round: int) -> List[Action]:
        if self.completed:
            return []

        self.completed = True
        self.__log(f"broadcasts final routing information after round {final_round}")
        self.__write_summary(final_round)
        return [
            self.__create_action(
                receiver_id=neighbor_id,
                message_type=self.FINAL,
                transaction_id=self.transaction_id,
                final_round=final_round,
            )
            for neighbor_id in self.neighbors
        ]

    def __write_summary(self, final_round: int) -> None:
        if self.summary_written:
            return

        self.summary_written = True
        self.summary_path.parent.mkdir(parents=True, exist_ok=True)
        with self.summary_path.open("a", encoding="utf-8") as output_file:
            output_file.write(
                f"node={self.__label(self.node_id)} "
                f"distance={self.__format_distance(self.dist)} "
                f"parent={self.__label(self.parent)} "
                f"initial_tree_parent={self.__label(self.initial_tree_parent)} "
                f"rounds={final_round}\n"
            )

    def __create_action(self, receiver_id: uuid.UUID, message_type: str, **extra_fields: Any) -> Action:
        payload = {
            "sender_id": self.node_id,
            "message_type": message_type,
        }
        payload.update(extra_fields)
        return Action(payload, node_id=receiver_id, action_id=uuid.uuid4())

    def __log(self, message: str) -> None:
        self.output_path.parent.mkdir(parents=True, exist_ok=True)
        with self.output_path.open("a", encoding="utf-8") as output_file:
            output_file.write(
                f"node={self.__label(self.node_id)} "
                f"dist={self.__format_distance(self.dist)} "
                f"parent={self.__label(self.parent)} "
                f"{message}\n"
            )

    @staticmethod
    def __label(node_id: Optional[uuid.UUID]) -> Optional[int]:
        if node_id is None:
            return None
        return node_id.int

    @staticmethod
    def __format_distance(distance: float) -> str:
        if math.isinf(distance):
            return "inf"
        return str(int(distance))
