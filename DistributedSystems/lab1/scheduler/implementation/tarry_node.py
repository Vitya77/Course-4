import random
import uuid
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Set

from scheduler.abstract.abstract_node import AbstractNode
from scheduler.core.action import Action
from scheduler.core.clocks import LamportClock, VectorClock
from scheduler.core.mailbox import Mailbox
from scheduler.core.node_response import NodeResponse


@dataclass
class TarryTraversalState:
    parent: Optional[uuid.UUID] = None
    visited_first_time: bool = False
    sent_to: Set[uuid.UUID] = field(default_factory=set)
    data: Any = None
    completed: bool = False


class TarryNode(AbstractNode):

    def __init__(self, node_id: uuid.UUID, neighbors: List[uuid.UUID], all_node_ids: List[uuid.UUID]):
        self.node_id = node_id
        self.mailbox = Mailbox()
        self.neighbors = neighbors
        self.data = None
        self.visited = 0
        self.lamport_clock = LamportClock()
        self.vector_clock = VectorClock(node_id, all_node_ids)
        self.traversals: Dict[uuid.UUID, TarryTraversalState] = {}

    def process_action(self, message: Action) -> NodeResponse:
        self.visited += 1
        self._register_receive_event(message)
        new_message = self.process_message(message)
        if new_message is None:
            return NodeResponse([])
        return NodeResponse([new_message])

    def process_message(self, message: Action) -> Optional[Action]:
        print(f"Node {self.node_id} processing {message}")
        if message.data.get('message_type') == 'New':
            outbox_messages = self.start_wave(message.data)
        elif message.data.get('message_type') == 'Offer':
            outbox_messages = self.receive_offer(message.data)
        else:
            raise ValueError(f"Unsupported message type: {message.data.get('message_type')}")
        print(f"Node {self.node_id} Data {self.data}")
        return outbox_messages

    def start_wave(self, message: dict[str, Any]) -> Action:
        transaction_id = message.get("transaction_id")
        state = self._get_or_create_state(transaction_id)
        transaction_data = message.get("transaction_data")
        receiver = random.choice(self.neighbors)
        self.data = transaction_data
        state.data = transaction_data
        state.visited_first_time = True
        state.sent_to.add(receiver)
        print(f"Node {self.node_id} STARTED ALGORITHM")
        return self._build_offer_action(receiver, transaction_id, transaction_data)

    def receive_offer(self, message: Dict[Any, Any]) -> Optional[Action]:
        transaction_id = message.get("transaction_id")
        state = self._get_or_create_state(transaction_id)
        if not state.visited_first_time:
            state.visited_first_time = True
            state.parent = message.get("sender_id")
            state.data = message.get("transaction_data")
            self.data = state.data
        receiver = None
        for neighbor in self.neighbors:
            if neighbor != state.parent and neighbor not in state.sent_to:
                receiver = neighbor
                state.sent_to.add(receiver)
                break
        if receiver is None and state.parent:
            receiver = state.parent
            state.completed = True
            print(f"Node {self.node_id} FINISHED")
        if receiver is None:
            state.completed = True
            print(f"Node {self.node_id} FINISHED ALGORITHM")
            return None
        return self._build_offer_action(receiver, transaction_id, state.data)

    def describe_state(self) -> str:
        active_transactions = sum(not state.completed for state in self.traversals.values())
        completed_transactions = sum(state.completed for state in self.traversals.values())
        return (
            f"Node {self.node_id}, Visited: {self.visited}, Data: {self.data}, "
            f"Lamport: {self.lamport_clock.snapshot()}, "
            f"Vector: {self._format_vector_clock(self.vector_clock.snapshot())}, "
            f"Active TX: {active_transactions}, Completed TX: {completed_transactions}"
        )

    def _register_receive_event(self, message: Action) -> None:
        clock_payload = message.data.get("clock")
        if clock_payload is None:
            lamport_timestamp = self.lamport_clock.tick()
            vector_timestamp = self.vector_clock.tick()
        else:
            lamport_timestamp = self.lamport_clock.update(clock_payload.get("lamport", 0))
            vector_timestamp = self.vector_clock.update(clock_payload.get("vector", {}))
        print(
            f"Node {self.node_id} RECEIVE clock "
            f"(Lamport={lamport_timestamp}, Vector={self._format_vector_clock(vector_timestamp)})"
        )

    def _build_offer_action(
        self,
        receiver: uuid.UUID,
        transaction_id: uuid.UUID,
        transaction_data: Any,
    ) -> Action:
        lamport_timestamp = self.lamport_clock.tick()
        vector_timestamp = self.vector_clock.tick()
        payload = {
            'sender_id': self.node_id,
            'transaction_id': transaction_id,
            'transaction_data': transaction_data,
            'message_type': "Offer",
            'clock': {
                'lamport': lamport_timestamp,
                'vector': vector_timestamp,
            }
        }
        print(
            f"Node {self.node_id} SEND -> {receiver} "
            f"(Lamport={lamport_timestamp}, Vector={self._format_vector_clock(vector_timestamp)})"
        )
        return Action(payload, receiver, uuid.uuid4())

    def _get_or_create_state(self, transaction_id: uuid.UUID) -> TarryTraversalState:
        if transaction_id not in self.traversals:
            self.traversals[transaction_id] = TarryTraversalState()
        return self.traversals[transaction_id]

    @staticmethod
    def _format_vector_clock(clock: Dict[str, int]) -> Dict[str, int]:
        return {node_id[:8]: timestamp for node_id, timestamp in clock.items()}
