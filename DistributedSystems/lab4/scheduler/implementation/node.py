import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional

from DistributedSystems.lab4.scheduler.abstract.abstract_node import AbstractNode
from DistributedSystems.lab4.scheduler.core.action import Action
from DistributedSystems.lab4.scheduler.core.mailbox import Mailbox
from DistributedSystems.lab4.scheduler.core.node_response import NodeResponse


@dataclass
class SnapshotState:
    started: bool = False
    completed: bool = False
    parent: Optional[uuid.UUID] = None
    children: set[uuid.UUID] = field(default_factory=set)
    processed_messages: List[dict[str, Any]] = field(default_factory=list)
    snapshot_recorded: bool = False
    snapshot_index: Optional[int] = None
    local_snapshot: Optional[dict[str, Any]] = None
    sent_false_count: Dict[uuid.UUID, int] = field(default_factory=dict)
    received_false_total: Dict[uuid.UUID, int] = field(default_factory=dict)
    baseline_false_received: Dict[uuid.UUID, int] = field(default_factory=dict)
    presnap_received: Dict[uuid.UUID, bool] = field(default_factory=dict)
    presnap_false_total: Dict[uuid.UUID, int] = field(default_factory=dict)
    expected_false_after_snapshot: Dict[uuid.UUID, int] = field(default_factory=dict)
    received_false_after_snapshot: Dict[uuid.UUID, int] = field(default_factory=dict)
    channel_state: Dict[uuid.UUID, List[dict[str, Any]]] = field(default_factory=dict)


class Node(AbstractNode):
    START = "New"
    BASE = "BASE"
    PRESNAP = "PRESNAP"

    def __init__(self, node_id: uuid.UUID, neighbors: List[uuid.UUID]):
        self.node_id = node_id
        self.mailbox = Mailbox()
        self.neighbors = sorted(neighbors, key=str)
        self.snapshots: Dict[uuid.UUID, SnapshotState] = {}
        self.output_path = Path("test_results/lai_yang_snapshots.txt")

    def process_action(self, message: Action) -> NodeResponse:
        transaction_id = message.data.get("transaction_id", message.action_id)
        message_type = message.data.get("message_type")
        state = self.__get_state(transaction_id)

        if message_type == self.START:
            return NodeResponse(self.__start_snapshot(transaction_id, message.data, state))
        if message_type == self.BASE:
            return NodeResponse(self.__handle_base_message(transaction_id, message.data, state))
        if message_type == self.PRESNAP:
            return NodeResponse(self.__handle_presnap_message(transaction_id, message.data, state))
        raise ValueError(f"Unsupported message type: {message_type}")

    def __get_state(self, transaction_id: uuid.UUID) -> SnapshotState:
        if transaction_id not in self.snapshots:
            state = SnapshotState()
            state.sent_false_count = {neighbor: 0 for neighbor in self.neighbors}
            state.received_false_total = {neighbor: 0 for neighbor in self.neighbors}
            state.baseline_false_received = {neighbor: 0 for neighbor in self.neighbors}
            state.presnap_received = {neighbor: False for neighbor in self.neighbors}
            state.presnap_false_total = {neighbor: 0 for neighbor in self.neighbors}
            state.expected_false_after_snapshot = {neighbor: 0 for neighbor in self.neighbors}
            state.received_false_after_snapshot = {neighbor: 0 for neighbor in self.neighbors}
            state.channel_state = {neighbor: [] for neighbor in self.neighbors}
            self.snapshots[transaction_id] = state
        return self.snapshots[transaction_id]

    def __start_snapshot(
        self,
        transaction_id: uuid.UUID,
        message_data: dict[str, Any],
        state: SnapshotState,
    ) -> List[Action]:
        if state.started:
            return []

        state.started = True
        transaction_data = message_data.get("transaction_data")
        actions = [
            self.__create_base_action(transaction_id, neighbor_id, transaction_data, is_post_snapshot=False)
            for neighbor_id in self.neighbors
        ]
        actions.extend(self.__record_snapshot_and_send_presnap(transaction_id, state))
        self.__try_complete_snapshot(transaction_id, state)
        return actions

    def __handle_base_message(
        self,
        transaction_id: uuid.UUID,
        message_data: dict[str, Any],
        state: SnapshotState,
    ) -> List[Action]:
        sender_id = message_data["sender_id"]
        is_post_snapshot = bool(message_data.get("snapshot_color", False))

        if not state.started:
            state.started = True
            state.parent = sender_id

        if not is_post_snapshot:
            state.received_false_total[sender_id] += 1

        if not state.snapshot_recorded and is_post_snapshot:
            self.__record_snapshot(transaction_id, state)

        if state.snapshot_recorded and not is_post_snapshot:
            state.channel_state[sender_id].append(
                {
                    "sender_id": str(sender_id),
                    "payload": message_data.get("transaction_data"),
                    "message_id": str(message_data.get("message_id")),
                }
            )
            state.received_false_after_snapshot[sender_id] += 1

        state.processed_messages.append(
            {
                "sender_id": str(sender_id),
                "payload": message_data.get("transaction_data"),
                "color": "red" if is_post_snapshot else "white",
            }
        )

        actions: List[Action] = []
        if sender_id not in state.children:
            state.children.add(sender_id)

        if len(state.processed_messages) == 1:
            actions.extend(
                self.__create_base_action(
                    transaction_id,
                    neighbor_id,
                    message_data.get("transaction_data"),
                    is_post_snapshot=state.snapshot_recorded,
                )
                for neighbor_id in self.neighbors
                if neighbor_id != sender_id
            )

        self.__try_complete_snapshot(transaction_id, state)
        return actions

    def __handle_presnap_message(
        self,
        transaction_id: uuid.UUID,
        message_data: dict[str, Any],
        state: SnapshotState,
    ) -> List[Action]:
        sender_id = message_data["sender_id"]
        false_messages_sent = int(message_data.get("false_messages_sent", 0))

        if not state.started:
            state.started = True
            state.parent = sender_id

        actions: List[Action] = []
        if not state.snapshot_recorded:
            actions.extend(self.__record_snapshot_and_send_presnap(transaction_id, state))

        state.presnap_received[sender_id] = True
        state.presnap_false_total[sender_id] = false_messages_sent
        baseline = state.baseline_false_received[sender_id]
        state.expected_false_after_snapshot[sender_id] = max(0, false_messages_sent - baseline)
        expected = state.expected_false_after_snapshot[sender_id]
        if len(state.channel_state[sender_id]) > expected:
            state.channel_state[sender_id] = state.channel_state[sender_id][:expected]
            state.received_false_after_snapshot[sender_id] = expected
        self.__try_complete_snapshot(transaction_id, state)
        return actions

    def __record_snapshot_and_send_presnap(
        self,
        transaction_id: uuid.UUID,
        state: SnapshotState,
    ) -> List[Action]:
        self.__record_snapshot(transaction_id, state)
        return [
            self.__create_presnap_action(
                transaction_id,
                neighbor_id,
                state.sent_false_count[neighbor_id],
            )
            for neighbor_id in self.neighbors
        ]

    def __record_snapshot(self, transaction_id: uuid.UUID, state: SnapshotState) -> None:
        if state.snapshot_recorded:
            return

        state.snapshot_recorded = True
        state.snapshot_index = len(state.processed_messages)
        state.baseline_false_received = dict(state.received_false_total)
        state.local_snapshot = {
            "node_id": str(self.node_id),
            "transaction_id": str(transaction_id),
            "processed_message_count": len(state.processed_messages),
            "processed_messages": list(state.processed_messages),
            "parent": str(state.parent) if state.parent else None,
            "neighbors": [str(neighbor_id) for neighbor_id in self.neighbors],
        }
        print(
            f"Node {self.node_id} recorded local snapshot for transaction {transaction_id}. "
            f"Processed messages: {len(state.processed_messages)}"
        )

    def __try_complete_snapshot(self, transaction_id: uuid.UUID, state: SnapshotState) -> None:
        if state.completed or not state.snapshot_recorded:
            return

        for neighbor_id in self.neighbors:
            if not state.presnap_received[neighbor_id]:
                return
            expected = state.expected_false_after_snapshot[neighbor_id]
            received = state.received_false_after_snapshot[neighbor_id]
            if received < expected:
                return

        state.completed = True
        self.__persist_snapshot(transaction_id, state)
        print(f"Node {self.node_id} completed Lai-Yang snapshot for transaction {transaction_id}")

    def __persist_snapshot(self, transaction_id: uuid.UUID, state: SnapshotState) -> None:
        self.output_path.parent.mkdir(parents=True, exist_ok=True)
        with self.output_path.open("a", encoding="utf-8") as output_file:
            output_file.write(
                f"transaction={transaction_id} "
                f"node={self.node_id} "
                f"snapshot={state.local_snapshot} "
                f"channels={{{', '.join(f'{str(sender)}: {messages}' for sender, messages in state.channel_state.items())}}}\n"
            )

    def __create_base_action(
        self,
        transaction_id: uuid.UUID,
        receiver_id: uuid.UUID,
        transaction_data: Any,
        is_post_snapshot: bool,
    ) -> Action:
        if not is_post_snapshot:
            state = self.__get_state(transaction_id)
            state.sent_false_count[receiver_id] += 1

        payload = {
            "sender_id": self.node_id,
            "transaction_id": transaction_id,
            "transaction_data": transaction_data,
            "message_type": self.BASE,
            "snapshot_color": is_post_snapshot,
            "message_id": uuid.uuid4(),
        }
        print(
            f"Node {self.node_id} sends {self.BASE} to {receiver_id} "
            f"for transaction {transaction_id} with color "
            f"{'red' if is_post_snapshot else 'white'}"
        )
        return Action(payload, node_id=receiver_id, action_id=uuid.uuid4())

    def __create_presnap_action(
        self,
        transaction_id: uuid.UUID,
        receiver_id: uuid.UUID,
        false_messages_sent: int,
    ) -> Action:
        payload = {
            "sender_id": self.node_id,
            "transaction_id": transaction_id,
            "message_type": self.PRESNAP,
            "false_messages_sent": false_messages_sent,
        }
        print(
            f"Node {self.node_id} sends {self.PRESNAP} to {receiver_id} "
            f"for transaction {transaction_id} with sent_white={false_messages_sent}"
        )
        return Action(payload, node_id=receiver_id, action_id=uuid.uuid4())
