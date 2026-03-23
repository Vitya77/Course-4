import uuid
from dataclasses import dataclass, field
from typing import Dict, List, Optional

from scheduler.abstract.abstract_node import AbstractNode
from scheduler.core.action import Action
from scheduler.core.mailbox import Mailbox
from scheduler.core.node_response import NodeResponse
from scheduler.settings.network_settings import settings


@dataclass
class AwerbuchState:
    root: bool = False
    visited: bool = False
    complete: bool = False
    parent: Optional[uuid.UUID] = None
    active_child: Optional[uuid.UUID] = None
    children: List[uuid.UUID] = field(default_factory=list)
    unexplored: List[uuid.UUID] = field(default_factory=list)
    pending_acks: set[uuid.UUID] = field(default_factory=set)


@dataclass
class CidonState:
    root: bool = False
    visited: bool = False
    complete: bool = False
    parent: Optional[uuid.UUID] = None
    active_child: Optional[uuid.UUID] = None
    children: List[uuid.UUID] = field(default_factory=list)
    remaining: List[uuid.UUID] = field(default_factory=list)
    informed_by_neighbors: set[uuid.UUID] = field(default_factory=set)
    frond_edges: set[uuid.UUID] = field(default_factory=set)
    info_sent: bool = False


class Node(AbstractNode):
    START = "New"

    AWERBUCH = "awerbuch"
    AWERBUCH_DISCOVER = "DISCOVER"
    AWERBUCH_VISITED = "VISITED"
    AWERBUCH_ACK = "ACK"
    AWERBUCH_RETURN = "RETURN"
    AWERBUCH_REJECT = "REJECT"

    CIDON = "cidon"
    CIDON_TOKEN = "TOKEN"
    CIDON_INFO = "INFO"
    CIDON_ACK = "INFO_ACK"

    def __init__(self, node_id: uuid.UUID, neighbors: List[uuid.UUID]):
        self.node_id = node_id
        self.mailbox = Mailbox()
        self.neighbors = sorted(neighbors, key=str)
        self.awerbuch_traversals: Dict[uuid.UUID, AwerbuchState] = {}
        self.cidon_traversals: Dict[uuid.UUID, CidonState] = {}

    def process_action(self, message: Action) -> NodeResponse:
        transaction_id = message.data.get("transaction_id", message.action_id)
        message_type = message.data.get("message_type")
        sender_id = message.data.get("sender_id")

        if settings.TRAVERSAL_ALGORITHM == self.AWERBUCH:
            return NodeResponse(
                self.__process_awerbuch_action(transaction_id, message_type, sender_id)
            )
        if settings.TRAVERSAL_ALGORITHM == self.CIDON:
            return NodeResponse(
                self.__process_cidon_action(transaction_id, message_type, sender_id)
            )
        raise ValueError(f"Unknown traversal algorithm: {settings.TRAVERSAL_ALGORITHM}")

    def __process_awerbuch_action(
        self,
        transaction_id: uuid.UUID,
        message_type: str,
        sender_id: Optional[uuid.UUID],
    ) -> List[Action]:
        state = self.__get_awerbuch_state(transaction_id)

        if message_type == self.START:
            return self.__start_awerbuch(transaction_id, state)
        if message_type == self.AWERBUCH_DISCOVER:
            return self.__handle_awerbuch_discover(transaction_id, state, sender_id)
        if message_type == self.AWERBUCH_VISITED:
            return self.__handle_awerbuch_visited(transaction_id, state, sender_id)
        if message_type == self.AWERBUCH_ACK:
            return self.__handle_awerbuch_ack(transaction_id, state, sender_id)
        if message_type == self.AWERBUCH_RETURN:
            return self.__handle_awerbuch_return(transaction_id, state, sender_id, accepted=True)
        if message_type == self.AWERBUCH_REJECT:
            return self.__handle_awerbuch_return(transaction_id, state, sender_id, accepted=False)
        raise ValueError(f"Unknown Awerbuch message type: {message_type}")

    def __get_awerbuch_state(self, transaction_id: uuid.UUID) -> AwerbuchState:
        if transaction_id not in self.awerbuch_traversals:
            self.awerbuch_traversals[transaction_id] = AwerbuchState(
                unexplored=list(self.neighbors)
            )
        return self.awerbuch_traversals[transaction_id]

    def __start_awerbuch(self, transaction_id: uuid.UUID, state: AwerbuchState) -> List[Action]:
        if state.visited:
            return []
        state.root = True
        state.visited = True
        state.parent = None
        state.pending_acks = set(state.unexplored)
        actions = [
            self.__create_action(transaction_id, neighbor_id, self.AWERBUCH_VISITED)
            for neighbor_id in state.unexplored
        ]
        return actions + self.__continue_awerbuch(transaction_id, state)

    def __handle_awerbuch_discover(
        self,
        transaction_id: uuid.UUID,
        state: AwerbuchState,
        sender_id: uuid.UUID,
    ) -> List[Action]:
        if state.visited:
            return [self.__create_action(transaction_id, sender_id, self.AWERBUCH_REJECT)]

        state.visited = True
        state.parent = sender_id
        state.root = False
        state.unexplored = [neighbor for neighbor in state.unexplored if neighbor != sender_id]
        state.pending_acks = set(state.unexplored)
        actions = [
            self.__create_action(transaction_id, neighbor_id, self.AWERBUCH_VISITED)
            for neighbor_id in state.unexplored
        ]
        return actions + self.__continue_awerbuch(transaction_id, state)

    def __handle_awerbuch_visited(
        self,
        transaction_id: uuid.UUID,
        state: AwerbuchState,
        sender_id: uuid.UUID,
    ) -> List[Action]:
        state.unexplored = [neighbor for neighbor in state.unexplored if neighbor != sender_id]
        return [self.__create_action(transaction_id, sender_id, self.AWERBUCH_ACK)]

    def __handle_awerbuch_ack(
        self,
        transaction_id: uuid.UUID,
        state: AwerbuchState,
        sender_id: uuid.UUID,
    ) -> List[Action]:
        state.pending_acks.discard(sender_id)
        return self.__continue_awerbuch(transaction_id, state)

    def __handle_awerbuch_return(
        self,
        transaction_id: uuid.UUID,
        state: AwerbuchState,
        sender_id: uuid.UUID,
        accepted: bool,
    ) -> List[Action]:
        if accepted and sender_id not in state.children:
            state.children.append(sender_id)
        if state.active_child == sender_id:
            state.active_child = None
        return self.__continue_awerbuch(transaction_id, state)

    def __continue_awerbuch(self, transaction_id: uuid.UUID, state: AwerbuchState) -> List[Action]:
        if state.complete or state.pending_acks or state.active_child is not None:
            return []

        if state.unexplored:
            next_neighbor = state.unexplored.pop(0)
            state.active_child = next_neighbor
            return [self.__create_action(transaction_id, next_neighbor, self.AWERBUCH_DISCOVER)]

        state.complete = True
        if state.root:
            return []
        return [self.__create_action(transaction_id, state.parent, self.AWERBUCH_RETURN)]

    def __process_cidon_action(
        self,
        transaction_id: uuid.UUID,
        message_type: str,
        sender_id: Optional[uuid.UUID],
    ) -> List[Action]:
        state = self.__get_cidon_state(transaction_id)

        if message_type == self.START:
            return self.__start_cidon(transaction_id, state)
        if message_type == self.CIDON_TOKEN:
            return self.__handle_cidon_token(transaction_id, state, sender_id)
        if message_type == self.CIDON_INFO:
            return self.__handle_cidon_info(transaction_id, state, sender_id)
        if message_type == self.CIDON_ACK:
            return []
        raise ValueError(f"Unknown Cidon message type: {message_type}")

    def __get_cidon_state(self, transaction_id: uuid.UUID) -> CidonState:
        if transaction_id not in self.cidon_traversals:
            self.cidon_traversals[transaction_id] = CidonState(
                remaining=list(self.neighbors)
            )
        return self.cidon_traversals[transaction_id]

    def __start_cidon(self, transaction_id: uuid.UUID, state: CidonState) -> List[Action]:
        if state.visited:
            return []
        state.root = True
        state.visited = True
        return self.__continue_cidon(transaction_id, state, first_visit=True)

    def __handle_cidon_token(
        self,
        transaction_id: uuid.UUID,
        state: CidonState,
        sender_id: uuid.UUID,
    ) -> List[Action]:
        if not state.visited:
            state.visited = True
            state.parent = sender_id
            state.remaining = [neighbor for neighbor in state.remaining if neighbor != sender_id]
            state.frond_edges.update(
                neighbor for neighbor in state.informed_by_neighbors if neighbor != sender_id
            )
            return self.__continue_cidon(transaction_id, state, first_visit=True)

        if state.active_child == sender_id:
            if sender_id not in state.children:
                state.children.append(sender_id)
            state.active_child = None
            return self.__continue_cidon(transaction_id, state, first_visit=False)

        state.frond_edges.add(sender_id)
        state.remaining = [neighbor for neighbor in state.remaining if neighbor != sender_id]
        return []

    def __handle_cidon_info(
        self,
        transaction_id: uuid.UUID,
        state: CidonState,
        sender_id: uuid.UUID,
    ) -> List[Action]:
        state.informed_by_neighbors.add(sender_id)
        actions = [self.__create_action(transaction_id, sender_id, self.CIDON_ACK)]

        if not state.visited:
            return actions

        if sender_id != state.parent:
            state.frond_edges.add(sender_id)
            state.remaining = [neighbor for neighbor in state.remaining if neighbor != sender_id]

        if state.active_child == sender_id:
            state.active_child = None
            return actions + self.__continue_cidon(transaction_id, state, first_visit=False)

        return actions

    def __continue_cidon(
        self,
        transaction_id: uuid.UUID,
        state: CidonState,
        first_visit: bool,
    ) -> List[Action]:
        if state.complete or state.active_child is not None:
            return []

        next_neighbor = self.__get_cidon_next_neighbor(state)
        actions: List[Action] = []

        if first_visit and not state.info_sent:
            informed_neighbors = [
                neighbor for neighbor in self.neighbors
                if neighbor != state.parent and neighbor != next_neighbor
            ]
            actions.extend(
                self.__create_action(transaction_id, neighbor, self.CIDON_INFO)
                for neighbor in informed_neighbors
            )
            state.info_sent = True

        if next_neighbor is not None:
            state.remaining = [neighbor for neighbor in state.remaining if neighbor != next_neighbor]
            state.active_child = next_neighbor
            actions.append(self.__create_action(transaction_id, next_neighbor, self.CIDON_TOKEN))
            return actions

        state.complete = True
        if state.root:
            return actions
        actions.append(self.__create_action(transaction_id, state.parent, self.CIDON_TOKEN))
        return actions

    def __get_cidon_next_neighbor(self, state: CidonState) -> Optional[uuid.UUID]:
        for neighbor in state.remaining:
            if neighbor == state.parent:
                continue
            if neighbor in state.frond_edges:
                continue
            return neighbor
        return None

    def __create_action(
        self,
        transaction_id: uuid.UUID,
        target_node_id: uuid.UUID,
        message_type: str,
    ) -> Action:
        return Action(
            {
                "transaction_id": transaction_id,
                "message_type": message_type,
                "sender_id": self.node_id,
            },
            node_id=target_node_id,
            action_id=transaction_id,
        )
