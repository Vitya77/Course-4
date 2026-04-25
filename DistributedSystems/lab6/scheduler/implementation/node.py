import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Set

from scheduler.abstract.abstract_node import AbstractNode
from scheduler.core.action import Action
from scheduler.core.clocks import LamportClock
from scheduler.core.mailbox import Mailbox
from scheduler.core.node_response import NodeResponse
from scheduler.settings.network_settings import settings


@dataclass
class RanaWaveState:
    initiator_id: uuid.UUID
    timestamp: int
    parent: Optional[uuid.UUID] = None
    initiated_here: bool = False
    pending_neighbors: Set[uuid.UUID] = field(default_factory=set)
    blocked: bool = False
    responded: bool = False
    completed: bool = False


class Node(AbstractNode):
    BASE_START = "BASE_START"
    BASE_WORK = "BASE_WORK"
    BASIC_ACK = "BASIC_ACK"

    RANA_WAVE = "RANA_WAVE"
    RANA_RESULT = "RANA_RESULT"

    SAFRA_TOKEN = "SAFRA_TOKEN"
    ANNOUNCE = "ANNOUNCE"

    WHITE = "white"
    BLACK = "black"

    def __init__(self, node_id: uuid.UUID, neighbors: List[uuid.UUID], all_node_ids: List[uuid.UUID]):
        self.node_id = node_id
        self.mailbox = Mailbox()
        self.neighbors = sorted(neighbors, key=lambda neighbor_id: neighbor_id.int)
        self.all_node_ids = sorted(all_node_ids, key=lambda current_id: current_id.int)
        self.output_path = Path("test_results/lab6_events.txt")
        self.summary_path = Path("test_results/lab6_summary.txt")

        self.clock = LamportClock()
        self.active = False
        self.outstanding_basic = 0
        self.quiet_since: Optional[int] = None
        self.last_started_rana_wave_at: Optional[int] = None
        self.rana_waves: Dict[str, RanaWaveState] = {}

        self.safra_counter = 0
        self.safra_color = self.WHITE
        self.safra_pending_token: Optional[dict[str, Any]] = None
        self.safra_started = False
        self.safra_round = 0

        self.processed_basic_messages = 0
        self.sent_basic_messages = 0
        self.received_basic_messages = 0

        self.termination_announced = False
        self.announcement_seen: Set[str] = set()
        self.announced_by: Optional[uuid.UUID] = None
        self.detection_algorithm: Optional[str] = None

        node_index = self.all_node_ids.index(self.node_id)
        self.ring_successor = self.all_node_ids[(node_index + 1) % len(self.all_node_ids)]
        self.ring_predecessor = self.all_node_ids[(node_index - 1) % len(self.all_node_ids)]
        self.is_safra_initiator = self.node_id == self.all_node_ids[0]

    def process_action(self, message: Action) -> NodeResponse:
        if self.termination_announced and message.data.get("message_type") != self.ANNOUNCE:
            return NodeResponse([])

        self.__register_receive_clock(message)
        message_type = message.data.get("message_type")

        if message_type == self.BASE_START:
            actions = self.__handle_base_start(message.data)
        elif message_type == self.BASE_WORK:
            actions = self.__handle_base_work(message.data)
        elif message_type == self.BASIC_ACK:
            actions = self.__handle_basic_ack(message.data)
        elif message_type == self.RANA_WAVE:
            actions = self.__handle_rana_wave(message.data)
        elif message_type == self.RANA_RESULT:
            actions = self.__handle_rana_result(message.data)
        elif message_type == self.SAFRA_TOKEN:
            actions = self.__handle_safra_token(message.data)
        elif message_type == self.ANNOUNCE:
            actions = self.__handle_announce(message.data)
        else:
            raise ValueError(f"Unsupported message type: {message_type}")

        actions.extend(self.__post_action_housekeeping())
        return NodeResponse(actions)

    def __handle_base_start(self, message_data: dict[str, Any]) -> List[Action]:
        self.__become_active("local start")
        self.__log(
            f"starts decentralized computation with remaining_hops={message_data['remaining_hops']}"
        )
        actions = self.__spawn_basic_messages(
            remaining_hops=int(message_data["remaining_hops"]),
            sender_id=None,
            transaction_id=message_data["transaction_id"],
            work_origin=int(message_data["work_origin"]),
        )
        self.__become_passive("local start processed")
        return actions

    def __handle_base_work(self, message_data: dict[str, Any]) -> List[Action]:
        sender_id = message_data["sender_id"]
        self.received_basic_messages += 1
        self.processed_basic_messages += 1

        if settings.TERMINATION_ALGORITHM == "safra":
            self.safra_counter -= 1
            self.safra_color = self.BLACK

        self.__become_active(
            f"received basic work from {self.__label(sender_id)} with remaining_hops={message_data['remaining_hops']}"
        )

        actions: List[Action] = []
        if settings.TERMINATION_ALGORITHM == "rana":
            actions.append(
                self.__create_action(
                    receiver_id=sender_id,
                    message_type=self.BASIC_ACK,
                    transaction_id=message_data["transaction_id"],
                    basic_message_id=message_data["basic_message_id"],
                )
            )

        actions.extend(
            self.__spawn_basic_messages(
                remaining_hops=int(message_data["remaining_hops"]),
                sender_id=sender_id,
                transaction_id=message_data["transaction_id"],
                work_origin=int(message_data["work_origin"]),
            )
        )
        self.__become_passive("basic work processed")
        return actions

    def __handle_basic_ack(self, message_data: dict[str, Any]) -> List[Action]:
        if self.outstanding_basic > 0:
            self.outstanding_basic -= 1
        self.__log(
            f"received ACK for basic message {message_data['basic_message_id']} "
            f"(outstanding={self.outstanding_basic})"
        )
        if not self.active and self.outstanding_basic == 0:
            self.quiet_since = self.clock.tick()
            self.__log(f"becomes quiet at t={self.quiet_since}: all ACKs received")
        return []

    def __handle_rana_wave(self, message_data: dict[str, Any]) -> List[Action]:
        wave_id = str(message_data["wave_id"])
        sender_id = message_data["sender_id"]
        initiator_id = message_data["initiator_id"]
        timestamp = int(message_data["wave_timestamp"])

        if wave_id in self.rana_waves:
            return [
                self.__create_action(
                    receiver_id=sender_id,
                    message_type=self.RANA_RESULT,
                    wave_id=wave_id,
                    status="duplicate",
                    transaction_id=message_data["transaction_id"],
                )
            ]

        if self.quiet_since is None or self.quiet_since > timestamp:
            self.__log(
                f"rejects Rana wave {wave_id} from {self.__label(sender_id)} "
                f"because quiet_since={self.quiet_since} > {timestamp}"
            )
            return [
                self.__create_action(
                    receiver_id=sender_id,
                    message_type=self.RANA_RESULT,
                    wave_id=wave_id,
                    status="blocked",
                    transaction_id=message_data["transaction_id"],
                )
            ]

        state = RanaWaveState(
            initiator_id=initiator_id,
            timestamp=timestamp,
            parent=sender_id,
            pending_neighbors={neighbor for neighbor in self.neighbors if neighbor != sender_id},
        )
        self.rana_waves[wave_id] = state
        self.__log(
            f"joins Rana wave {wave_id} from {self.__label(sender_id)} tagged with t={timestamp}"
        )

        if not state.pending_neighbors:
            state.responded = True
            state.completed = True
            return [
                self.__create_action(
                    receiver_id=sender_id,
                    message_type=self.RANA_RESULT,
                    wave_id=wave_id,
                    status="success",
                    transaction_id=message_data["transaction_id"],
                )
            ]

        return [
            self.__create_action(
                receiver_id=neighbor_id,
                message_type=self.RANA_WAVE,
                transaction_id=message_data["transaction_id"],
                wave_id=wave_id,
                initiator_id=initiator_id,
                wave_timestamp=timestamp,
            )
            for neighbor_id in sorted(state.pending_neighbors, key=lambda neighbor_id: neighbor_id.int)
        ]

    def __handle_rana_result(self, message_data: dict[str, Any]) -> List[Action]:
        wave_id = str(message_data["wave_id"])
        sender_id = message_data["sender_id"]
        status = message_data["status"]
        state = self.rana_waves.get(wave_id)
        if state is None or state.responded:
            return []

        state.pending_neighbors.discard(sender_id)
        if status == "blocked":
            state.blocked = True

        if state.pending_neighbors:
            return []

        state.completed = True
        state.responded = True

        if state.initiated_here:
            if not state.blocked:
                self.__log(
                    f"Rana wave {wave_id} completed successfully at t={state.timestamp}; announce termination"
                )
                return self.__announce("rana")
            self.__log(f"Rana wave {wave_id} did not complete")
            return []

        return [
            self.__create_action(
                receiver_id=state.parent,
                message_type=self.RANA_RESULT,
                wave_id=wave_id,
                status="blocked" if state.blocked else "success",
                transaction_id=message_data["transaction_id"],
            )
        ]

    def __handle_safra_token(self, message_data: dict[str, Any]) -> List[Action]:
        self.safra_pending_token = dict(message_data)
        self.__log(
            f"received Safra token round={message_data['round']} "
            f"color={message_data['token_color']} sum={message_data['token_sum']}"
        )
        return self.__try_forward_safra_token()

    def __handle_announce(self, message_data: dict[str, Any]) -> List[Action]:
        announcement_id = str(message_data["announcement_id"])
        if announcement_id in self.announcement_seen:
            return []

        self.announcement_seen.add(announcement_id)
        self.termination_announced = True
        self.announced_by = message_data["detected_by"]
        self.detection_algorithm = message_data["algorithm"]

        self.summary_path.parent.mkdir(parents=True, exist_ok=True)
        with self.summary_path.open("a", encoding="utf-8") as output_file:
            output_file.write(
                f"node={self.__label(self.node_id)} "
                f"algorithm={self.detection_algorithm} "
                f"announced_by={self.__label(self.announced_by)} "
                f"processed={self.processed_basic_messages} "
                f"sent={self.sent_basic_messages} "
                f"received={self.received_basic_messages}\n"
            )

        self.__log(
            f"accepted termination announcement from {self.__label(message_data['sender_id'])} "
            f"detected_by={self.__label(self.announced_by)} algorithm={self.detection_algorithm}"
        )

        sender_id = message_data["sender_id"]
        return [
            self.__create_action(
                receiver_id=neighbor_id,
                message_type=self.ANNOUNCE,
                announcement_id=announcement_id,
                detected_by=self.announced_by,
                algorithm=self.detection_algorithm,
            )
            for neighbor_id in self.neighbors
            if neighbor_id != sender_id
        ]

    def __spawn_basic_messages(
        self,
        remaining_hops: int,
        sender_id: Optional[uuid.UUID],
        transaction_id: uuid.UUID,
        work_origin: int,
    ) -> List[Action]:
        if remaining_hops <= 0:
            return []

        candidates = [neighbor for neighbor in self.neighbors if neighbor != sender_id]
        if not candidates:
            return []

        rotation = self.processed_basic_messages % len(candidates)
        ordered_candidates = candidates[rotation:] + candidates[:rotation]
        fan_out = min(settings.MAX_FORWARD_PER_STEP, len(ordered_candidates))

        actions: List[Action] = []
        for receiver_id in ordered_candidates[:fan_out]:
            basic_message_id = uuid.uuid4()
            self.sent_basic_messages += 1
            if settings.TERMINATION_ALGORITHM == "rana":
                self.outstanding_basic += 1
            if settings.TERMINATION_ALGORITHM == "safra":
                self.safra_counter += 1
            payload = {
                "transaction_id": transaction_id,
                "remaining_hops": remaining_hops - 1,
                "work_origin": work_origin,
                "basic_message_id": basic_message_id,
            }
            actions.append(
                self.__create_action(
                    receiver_id=receiver_id,
                    message_type=self.BASE_WORK,
                    **payload,
                )
            )
            self.__log(
                f"sends basic work {basic_message_id} to {self.__label(receiver_id)} "
                f"(remaining_hops={remaining_hops - 1}, outstanding={self.outstanding_basic}, "
                f"safra_counter={self.safra_counter})"
            )
        return actions

    def __post_action_housekeeping(self) -> List[Action]:
        actions: List[Action] = []
        if settings.TERMINATION_ALGORITHM == "rana":
            actions.extend(self.__flush_blocked_rana_waves())
            actions.extend(self.__maybe_start_rana_wave())
        if settings.TERMINATION_ALGORITHM == "safra":
            actions.extend(self.__try_forward_safra_token())
            actions.extend(self.__maybe_start_safra_round())
        return actions

    def __maybe_start_rana_wave(self) -> List[Action]:
        if self.termination_announced or self.quiet_since is None:
            return []
        if self.last_started_rana_wave_at == self.quiet_since:
            return []

        self.last_started_rana_wave_at = self.quiet_since
        wave_id = str(uuid.uuid4())
        state = RanaWaveState(
            initiator_id=self.node_id,
            timestamp=self.quiet_since,
            initiated_here=True,
            pending_neighbors=set(self.neighbors),
        )
        self.rana_waves[wave_id] = state
        self.__log(f"starts Rana wave {wave_id} at quiet_time={self.quiet_since}")

        if not state.pending_neighbors:
            state.completed = True
            state.responded = True
            return self.__announce("rana")

        return [
            self.__create_action(
                receiver_id=neighbor_id,
                message_type=self.RANA_WAVE,
                transaction_id=uuid.uuid4(),
                wave_id=wave_id,
                initiator_id=self.node_id,
                wave_timestamp=self.quiet_since,
            )
            for neighbor_id in sorted(self.neighbors, key=lambda neighbor_id: neighbor_id.int)
        ]

    def __maybe_start_safra_round(self) -> List[Action]:
        if settings.TERMINATION_ALGORITHM != "safra":
            return []
        if self.termination_announced or not self.is_safra_initiator or self.active:
            return []
        if self.safra_pending_token is not None:
            return []
        if self.safra_started:
            return []

        self.safra_started = True
        self.safra_round += 1
        self.__log(f"starts Safra token round {self.safra_round}")
        return [
            self.__create_action(
                receiver_id=self.ring_successor,
                message_type=self.SAFRA_TOKEN,
                round=self.safra_round,
                token_sum=0,
                token_color=self.WHITE,
            )
        ]

    def __try_forward_safra_token(self) -> List[Action]:
        if settings.TERMINATION_ALGORITHM != "safra":
            return []
        if self.safra_pending_token is None or self.active or self.termination_announced:
            return []

        token = self.safra_pending_token
        self.safra_pending_token = None

        if self.is_safra_initiator and token["sender_id"] == self.ring_predecessor:
            return self.__handle_returned_safra_token(token)

        token_sum = int(token["token_sum"]) + self.safra_counter
        token_color = token["token_color"]
        if self.safra_color == self.BLACK:
            token_color = self.BLACK
            self.safra_color = self.WHITE
        self.safra_counter = 0

        self.__log(
            f"forwards Safra token round={token['round']} "
            f"sum={token_sum} color={token_color}"
        )
        return [
            self.__create_action(
                receiver_id=self.ring_successor,
                message_type=self.SAFRA_TOKEN,
                round=int(token["round"]),
                token_sum=token_sum,
                token_color=token_color,
            )
        ]

    def __handle_returned_safra_token(self, token: dict[str, Any]) -> List[Action]:
        initiator_was_black = self.safra_color == self.BLACK
        total_sum = int(token["token_sum"]) + self.safra_counter
        token_was_white = token["token_color"] == self.WHITE

        self.__log(
            f"Safra token returned: round={token['round']} total_sum={total_sum} "
            f"token_color={token['token_color']} initiator_color={self.safra_color}"
        )

        self.safra_counter = 0
        self.safra_color = self.WHITE

        if token_was_white and not initiator_was_black and total_sum == 0:
            self.__log("Safra detected global termination")
            return self.__announce("safra")

        self.safra_round = max(self.safra_round, int(token["round"])) + 1
        self.__log(f"starts new Safra round {self.safra_round}")
        return [
            self.__create_action(
                receiver_id=self.ring_successor,
                message_type=self.SAFRA_TOKEN,
                round=self.safra_round,
                token_sum=0,
                token_color=self.WHITE,
            )
        ]

    def __announce(self, algorithm: str) -> List[Action]:
        announcement_id = str(uuid.uuid4())
        self.announcement_seen.add(announcement_id)
        self.termination_announced = True
        self.announced_by = self.node_id
        self.detection_algorithm = algorithm

        self.summary_path.parent.mkdir(parents=True, exist_ok=True)
        with self.summary_path.open("a", encoding="utf-8") as output_file:
            output_file.write(
                f"node={self.__label(self.node_id)} "
                f"algorithm={algorithm} "
                f"role=detector "
                f"processed={self.processed_basic_messages} "
                f"sent={self.sent_basic_messages} "
                f"received={self.received_basic_messages}\n"
            )

        self.__log(f"ANNOUNCE termination by {algorithm}")
        return [
            self.__create_action(
                receiver_id=neighbor_id,
                message_type=self.ANNOUNCE,
                announcement_id=announcement_id,
                detected_by=self.node_id,
                algorithm=algorithm,
            )
            for neighbor_id in self.neighbors
        ]

    def __become_active(self, reason: str) -> None:
        self.active = True
        self.quiet_since = None
        self.__invalidate_rana_waves()
        self.__log(f"becomes active: {reason}")

    def __become_passive(self, reason: str) -> None:
        self.active = False
        if settings.TERMINATION_ALGORITHM == "rana":
            if self.outstanding_basic == 0:
                self.quiet_since = self.clock.tick()
                self.__log(f"becomes quiet at t={self.quiet_since}: {reason}")
            else:
                self.__log(
                    f"becomes passive but not quiet (outstanding={self.outstanding_basic}): {reason}"
                )
        else:
            self.__log(f"becomes passive: {reason}")

    def __invalidate_rana_waves(self) -> None:
        if settings.TERMINATION_ALGORITHM != "rana":
            return

        for wave_id, state in self.rana_waves.items():
            if state.responded or state.completed:
                continue
            state.blocked = True
            if state.initiated_here:
                self.__log(f"local activity invalidated Rana wave {wave_id}")
                continue

    def __flush_blocked_rana_waves(self) -> List[Action]:
        actions: List[Action] = []
        for wave_id, state in self.rana_waves.items():
            if state.responded or state.completed or not state.blocked or state.initiated_here:
                continue
            state.responded = True
            state.completed = True
            actions.append(
                self.__create_action(
                    receiver_id=state.parent,
                    message_type=self.RANA_RESULT,
                    wave_id=wave_id,
                    status="blocked",
                    transaction_id=uuid.uuid4(),
                )
            )
        return actions

    def __register_receive_clock(self, message: Action) -> None:
        received_clock = int(message.data.get("clock", 0))
        updated = self.clock.update(received_clock)
        self.__log(
            f"received {message.data.get('message_type')} from "
            f"{self.__label(message.data.get('sender_id'))} clock={updated}"
        )

    def __create_action(self, receiver_id: uuid.UUID, message_type: str, **extra_fields: Any) -> Action:
        payload = {
            "sender_id": self.node_id,
            "message_type": message_type,
            "clock": self.clock.tick(),
        }
        payload.update(extra_fields)
        return Action(payload, node_id=receiver_id, action_id=uuid.uuid4())

    def __log(self, message: str) -> None:
        self.output_path.parent.mkdir(parents=True, exist_ok=True)
        with self.output_path.open("a", encoding="utf-8") as output_file:
            output_file.write(
                f"node={self.__label(self.node_id)} "
                f"clock={self.clock.snapshot()} "
                f"algorithm={settings.TERMINATION_ALGORITHM} "
                f"{message}\n"
            )

    @staticmethod
    def __label(node_id: Optional[uuid.UUID]) -> Optional[int]:
        if node_id is None:
            return None
        return node_id.int
