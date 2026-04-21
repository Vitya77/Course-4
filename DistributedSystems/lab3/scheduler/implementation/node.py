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
class EchoState:
    started: bool = False
    initiator: bool = False
    visited: bool = False
    parent: Optional[uuid.UUID] = None
    payload: Any = None
    pending_replies: Set[uuid.UUID] = field(default_factory=set)
    children: Set[uuid.UUID] = field(default_factory=set)
    decided: bool = False
    completed: bool = False
    persisted: bool = False


@dataclass
class TreeWaveState:
    started: bool = False
    initiator: bool = False
    parent: Optional[uuid.UUID] = None
    payload: Any = None
    wave_received_from: Set[uuid.UUID] = field(default_factory=set)
    wave_sent: bool = False
    decided: bool = False
    notifications_seen: Set[uuid.UUID] = field(default_factory=set)
    completed: bool = False
    persisted: bool = False


class Node(AbstractNode):
    START = "New"

    ECHO = "echo"
    ECHO_WAVE = "ECHO_WAVE"
    ECHO_REPLY = "ECHO_REPLY"

    TREE = "tree"
    TREE_WAVE = "TREE_WAVE"
    TREE_NOTIFY = "TREE_NOTIFY"

    def __init__(self, node_id: uuid.UUID, neighbors: List[uuid.UUID]):
        self.node_id = node_id
        self.mailbox = Mailbox()
        self.neighbors = sorted(neighbors, key=lambda neighbor_id: neighbor_id.int)
        self.echo_transactions: Dict[uuid.UUID, EchoState] = {}
        self.tree_transactions: Dict[uuid.UUID, TreeWaveState] = {}
        self.output_path = Path("test_results/lab3_results.txt")

    def process_action(self, message: Action) -> NodeResponse:
        transaction_id = message.data.get("transaction_id", message.action_id)
        message_type = message.data.get("message_type")

        if settings.WAVE_ALGORITHM == self.ECHO:
            return NodeResponse(
                self.__process_echo_action(transaction_id, message_type, message.data)
            )
        if settings.WAVE_ALGORITHM == self.TREE:
            return NodeResponse(
                self.__process_tree_action(transaction_id, message_type, message.data)
            )
        raise ValueError(f"Unsupported wave algorithm: {settings.WAVE_ALGORITHM}")

    def __process_echo_action(
        self,
        transaction_id: uuid.UUID,
        message_type: str,
        message_data: dict[str, Any],
    ) -> List[Action]:
        state = self.__get_echo_state(transaction_id)

        if message_type == self.START:
            return self.__start_echo(transaction_id, message_data, state)
        if message_type == self.ECHO_WAVE:
            return self.__handle_echo_wave(transaction_id, message_data, state)
        if message_type == self.ECHO_REPLY:
            return self.__handle_echo_reply(transaction_id, message_data, state)
        raise ValueError(f"Unsupported echo message type: {message_type}")

    def __get_echo_state(self, transaction_id: uuid.UUID) -> EchoState:
        if transaction_id not in self.echo_transactions:
            self.echo_transactions[transaction_id] = EchoState()
        return self.echo_transactions[transaction_id]

    def __start_echo(
        self,
        transaction_id: uuid.UUID,
        message_data: dict[str, Any],
        state: EchoState,
    ) -> List[Action]:
        if state.started:
            return []

        state.started = True
        state.initiator = True
        state.visited = True
        state.payload = message_data.get("transaction_data")
        state.pending_replies = set(self.neighbors)

        print(
            f"[Echo] Node {self.__label(self.node_id)} starts transaction {transaction_id} "
            f"and sends the wave to {[self.__label(neighbor) for neighbor in self.neighbors]}"
        )

        actions = [
            self.__create_action(transaction_id, neighbor_id, self.ECHO_WAVE, state.payload)
            for neighbor_id in self.neighbors
        ]

        if not state.pending_replies:
            state.decided = True
            state.completed = True
            self.__persist_echo_result(transaction_id, state)

        return actions

    def __handle_echo_wave(
        self,
        transaction_id: uuid.UUID,
        message_data: dict[str, Any],
        state: EchoState,
    ) -> List[Action]:
        sender_id = message_data["sender_id"]

        if not state.visited:
            state.started = True
            state.visited = True
            state.parent = sender_id
            state.payload = message_data.get("transaction_data")
            state.pending_replies = set(self.neighbors) - {sender_id}

            print(
                f"[Echo] Node {self.__label(self.node_id)} picks "
                f"{self.__label(sender_id)} as parent and forwards the wave to "
                f"{[self.__label(neighbor) for neighbor in sorted(state.pending_replies, key=lambda node_id: node_id.int)]}"
            )

            if not state.pending_replies:
                state.completed = True
                self.__persist_echo_result(transaction_id, state)
                return [
                    self.__create_action(
                        transaction_id,
                        sender_id,
                        self.ECHO_REPLY,
                        state.payload,
                        accepted=True,
                    )
                ]

            return [
                self.__create_action(transaction_id, neighbor_id, self.ECHO_WAVE, state.payload)
                for neighbor_id in sorted(state.pending_replies, key=lambda node_id: node_id.int)
            ]

        print(
            f"[Echo] Node {self.__label(self.node_id)} already visited transaction {transaction_id}, "
            f"so it immediately replies to {self.__label(sender_id)}"
        )
        return [
            self.__create_action(
                transaction_id,
                sender_id,
                self.ECHO_REPLY,
                state.payload,
                accepted=False,
            )
        ]

    def __handle_echo_reply(
        self,
        transaction_id: uuid.UUID,
        message_data: dict[str, Any],
        state: EchoState,
    ) -> List[Action]:
        sender_id = message_data["sender_id"]
        accepted = bool(message_data.get("accepted", False))

        if sender_id in state.pending_replies:
            state.pending_replies.remove(sender_id)
            if accepted:
                state.children.add(sender_id)

        if state.pending_replies:
            print(
                f"[Echo] Node {self.__label(self.node_id)} is still waiting for "
                f"{[self.__label(neighbor) for neighbor in sorted(state.pending_replies, key=lambda node_id: node_id.int)]}"
            )
            return []

        if state.initiator:
            state.decided = True
            state.completed = True
            print(
                f"[Echo] Initiator {self.__label(self.node_id)} collected all replies "
                f"for transaction {transaction_id} and decides"
            )
            self.__persist_echo_result(transaction_id, state)
            return []

        if state.completed:
            return []

        state.completed = True
        self.__persist_echo_result(transaction_id, state)
        print(
            f"[Echo] Node {self.__label(self.node_id)} sends the echo back to "
            f"{self.__label(state.parent)}"
        )
        return [
            self.__create_action(
                transaction_id,
                state.parent,
                self.ECHO_REPLY,
                state.payload,
                accepted=True,
            )
        ]

    def __process_tree_action(
        self,
        transaction_id: uuid.UUID,
        message_type: str,
        message_data: dict[str, Any],
    ) -> List[Action]:
        state = self.__get_tree_state(transaction_id)

        if message_type == self.START:
            return self.__start_tree(transaction_id, message_data, state)
        if message_type == self.TREE_WAVE:
            return self.__handle_tree_wave(transaction_id, message_data, state)
        if message_type == self.TREE_NOTIFY:
            return self.__handle_tree_notify(transaction_id, message_data, state)
        raise ValueError(f"Unsupported tree-wave message type: {message_type}")

    def __get_tree_state(self, transaction_id: uuid.UUID) -> TreeWaveState:
        if transaction_id not in self.tree_transactions:
            self.tree_transactions[transaction_id] = TreeWaveState()
        return self.tree_transactions[transaction_id]

    def __start_tree(
        self,
        transaction_id: uuid.UUID,
        message_data: dict[str, Any],
        state: TreeWaveState,
    ) -> List[Action]:
        if state.started:
            return []
        if len(self.neighbors) != 1:
            raise ValueError("The tree-wave algorithm must be initiated by leaf nodes only")

        state.started = True
        state.initiator = True
        state.parent = self.neighbors[0]
        state.payload = message_data.get("transaction_data")
        state.wave_sent = True

        print(
            f"[Tree] Leaf {self.__label(self.node_id)} starts transaction {transaction_id} "
            f"and sends the first wave to {self.__label(state.parent)}"
        )

        return [self.__create_action(transaction_id, state.parent, self.TREE_WAVE, state.payload)]

    def __handle_tree_wave(
        self,
        transaction_id: uuid.UUID,
        message_data: dict[str, Any],
        state: TreeWaveState,
    ) -> List[Action]:
        sender_id = message_data["sender_id"]
        state.started = True
        if state.payload is None:
            state.payload = message_data.get("transaction_data")

        if state.wave_sent and state.parent == sender_id:
            return self.__decide_tree(transaction_id, state)

        if sender_id in state.wave_received_from:
            return []

        state.wave_received_from.add(sender_id)
        remaining_neighbors = [
            neighbor_id
            for neighbor_id in self.neighbors
            if neighbor_id not in state.wave_received_from
        ]

        print(
            f"[Tree] Node {self.__label(self.node_id)} received a wave from "
            f"{self.__label(sender_id)}. Remaining candidates: "
            f"{[self.__label(neighbor) for neighbor in remaining_neighbors]}"
        )

        if not state.wave_sent and len(remaining_neighbors) == 1:
            state.parent = remaining_neighbors[0]
            state.wave_sent = True
            print(
                f"[Tree] Node {self.__label(self.node_id)} selects "
                f"{self.__label(state.parent)} as parent and forwards the wave"
            )
            return [self.__create_action(transaction_id, state.parent, self.TREE_WAVE, state.payload)]

        return []

    def __decide_tree(
        self,
        transaction_id: uuid.UUID,
        state: TreeWaveState,
    ) -> List[Action]:
        if state.decided:
            return []

        state.decided = True
        state.notifications_seen.add(self.node_id)

        print(
            f"[Tree] Node {self.__label(self.node_id)} received the wave from its parent "
            f"and becomes one of the two decision nodes"
        )

        actions = [
            self.__create_action(
                transaction_id,
                neighbor_id,
                self.TREE_NOTIFY,
                state.payload,
                decider_id=self.node_id,
            )
            for neighbor_id in self.neighbors
        ]

        if len(state.notifications_seen) >= 2:
            state.completed = True
            self.__persist_tree_result(transaction_id, state)

        return actions

    def __handle_tree_notify(
        self,
        transaction_id: uuid.UUID,
        message_data: dict[str, Any],
        state: TreeWaveState,
    ) -> List[Action]:
        sender_id = message_data["sender_id"]
        decider_id = message_data["decider_id"]
        state.started = True
        if state.payload is None:
            state.payload = message_data.get("transaction_data")

        if decider_id in state.notifications_seen:
            return []

        state.notifications_seen.add(decider_id)
        print(
            f"[Tree] Node {self.__label(self.node_id)} learned about decision node "
            f"{self.__label(decider_id)} from {self.__label(sender_id)}"
        )

        actions = [
            self.__create_action(
                transaction_id,
                neighbor_id,
                self.TREE_NOTIFY,
                state.payload,
                decider_id=decider_id,
            )
            for neighbor_id in self.neighbors
            if neighbor_id != sender_id
        ]

        if len(state.notifications_seen) >= 2:
            state.completed = True
            self.__persist_tree_result(transaction_id, state)

        return actions

    def __persist_echo_result(self, transaction_id: uuid.UUID, state: EchoState) -> None:
        if state.persisted:
            return
        state.persisted = True

        summary = {
            "initiator": state.initiator,
            "decided": state.decided,
            "parent": self.__label(state.parent) if state.parent else None,
            "children": [self.__label(child) for child in sorted(state.children, key=lambda node_id: node_id.int)],
            "pending_replies": [self.__label(child) for child in sorted(state.pending_replies, key=lambda node_id: node_id.int)],
        }
        self.__write_result("echo", transaction_id, summary)

    def __persist_tree_result(self, transaction_id: uuid.UUID, state: TreeWaveState) -> None:
        if state.persisted:
            return
        state.persisted = True

        summary = {
            "initiator": state.initiator,
            "decided": state.decided,
            "parent": self.__label(state.parent) if state.parent else None,
            "wave_received_from": [
                self.__label(node_id)
                for node_id in sorted(state.wave_received_from, key=lambda node_id: node_id.int)
            ],
            "decisions_seen": [
                self.__label(node_id)
                for node_id in sorted(state.notifications_seen, key=lambda node_id: node_id.int)
            ],
        }
        self.__write_result("tree", transaction_id, summary)

    def __write_result(
        self,
        algorithm: str,
        transaction_id: uuid.UUID,
        summary: dict[str, Any],
    ) -> None:
        self.output_path.parent.mkdir(parents=True, exist_ok=True)
        with self.output_path.open("a", encoding="utf-8") as output_file:
            output_file.write(
                f"algorithm={algorithm} "
                f"transaction={transaction_id} "
                f"node={self.__label(self.node_id)} "
                f"state={summary}\n"
            )

    def __create_action(
        self,
        transaction_id: uuid.UUID,
        receiver_id: uuid.UUID,
        message_type: str,
        transaction_data: Any,
        **extra_fields: Any,
    ) -> Action:
        payload = {
            "sender_id": self.node_id,
            "transaction_id": transaction_id,
            "transaction_data": transaction_data,
            "message_type": message_type,
        }
        payload.update(extra_fields)
        return Action(payload, node_id=receiver_id, action_id=uuid.uuid4())

    @staticmethod
    def __label(node_id: Optional[uuid.UUID]) -> Optional[int]:
        if node_id is None:
            return None
        return node_id.int
