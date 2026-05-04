import heapq
import math
import uuid
from pathlib import Path
from typing import Dict, List, Optional

from scheduler.abstract.abstract_network import AbstractNetwork
from scheduler.core.action import Action
from scheduler.implementation.node import Node
from scheduler.settings.network_settings import settings


class CurrentNetwork(AbstractNetwork):
    def __init__(self) -> None:
        self.nodes: List[Node] = []
        ids = [uuid.UUID(int=index + 1) for index in range(6)]
        self.node_labels = {index + 1: node_id for index, node_id in enumerate(ids)}
        self.initiator_id = self.node_labels[settings.INITIATOR_LABEL]
        self.weights = self.__build_weights()

        for node_id in ids:
            self.nodes.append(
                Node(
                    node_id=node_id,
                    weights=self.weights[node_id],
                    all_node_ids=ids,
                    initiator_id=self.initiator_id,
                )
            )

        super().__init__(self.nodes)
        self.summary_written = False
        self.__prepare_output_files()
        self.__seed_initial_action()

    def __build_weights(self) -> Dict[uuid.UUID, Dict[uuid.UUID, int]]:
        weights = {node_id: {} for node_id in self.node_labels.values()}

        def connect(left: int, right: int, cost: int) -> None:
            left_id = self.node_labels[left]
            right_id = self.node_labels[right]
            weights[left_id][right_id] = cost
            weights[right_id][left_id] = cost

        connect(1, 2, 7)
        connect(1, 3, 9)
        connect(1, 6, 14)
        connect(2, 3, 10)
        connect(2, 4, 15)
        connect(3, 4, 11)
        connect(3, 6, 2)
        connect(4, 5, 6)
        connect(5, 6, 9)
        return weights

    def __prepare_output_files(self) -> None:
        results_dir = Path("test_results")
        results_dir.mkdir(parents=True, exist_ok=True)
        Path(settings.OUTPUT_EVENTS_PATH).write_text("", encoding="utf-8")
        Path(settings.OUTPUT_SUMMARY_PATH).write_text("", encoding="utf-8")
        Path(settings.OUTPUT_VERIFICATION_PATH).write_text("", encoding="utf-8")

    def __seed_initial_action(self) -> None:
        payload = {
            "message_type": Node.START,
            "transaction_id": uuid.uuid4(),
        }
        for node in self.nodes:
            if node.node_id == self.initiator_id:
                node.mailbox.add_inbox_action(
                    Action(payload, node_id=self.initiator_id, action_id=uuid.uuid4())
                )
                break

    def is_finished(self) -> bool:
        nodes_completed = all(node.is_completed() for node in self.nodes)
        has_actions = any(node.mailbox.get_actions() for node in self.nodes)
        if nodes_completed and not has_actions and not self.summary_written:
            self.__write_verification()
            self.summary_written = True
        return nodes_completed and not has_actions

    def __write_verification(self) -> None:
        expected_distances, expected_parents = self.__expected_shortest_paths()
        verification_path = Path(settings.OUTPUT_VERIFICATION_PATH)
        with verification_path.open("w", encoding="utf-8") as output_file:
            for node in sorted(self.nodes, key=lambda current_node: current_node.node_id.int):
                expected_dist = expected_distances[node.node_id]
                expected_parent = expected_parents[node.node_id]
                actual = node.summary_data()
                actual_distance = actual["distance"]
                actual_parent = actual["parent"]
                output_file.write(
                    f"node={actual['node']} "
                    f"expected_distance={self.__format_distance(expected_dist)} "
                    f"actual_distance={actual_distance} "
                    f"expected_parent={self.__label(expected_parent)} "
                    f"actual_parent={actual_parent} "
                    f"status={'OK' if actual_distance == self.__to_int(expected_dist) and actual_parent == self.__label(expected_parent) else 'MISMATCH'}\n"
                )

    def __expected_shortest_paths(self) -> tuple[Dict[uuid.UUID, float], Dict[uuid.UUID, Optional[uuid.UUID]]]:
        distances = {node_id: math.inf for node_id in self.weights}
        parents: Dict[uuid.UUID, Optional[uuid.UUID]] = {node_id: None for node_id in self.weights}
        distances[self.initiator_id] = 0
        heap: List[tuple[float, int, uuid.UUID]] = [(0, self.initiator_id.int, self.initiator_id)]

        while heap:
            current_distance, _, node_id = heapq.heappop(heap)
            if current_distance > distances[node_id]:
                continue
            for neighbor_id, cost in self.weights[node_id].items():
                candidate = current_distance + cost
                if candidate < distances[neighbor_id]:
                    distances[neighbor_id] = candidate
                    parents[neighbor_id] = node_id
                    heapq.heappush(heap, (candidate, neighbor_id.int, neighbor_id))
        return distances, parents

    @staticmethod
    def __label(node_id: Optional[uuid.UUID]) -> Optional[int]:
        if node_id is None:
            return None
        return node_id.int

    @staticmethod
    def __format_distance(distance: float) -> str:
        return "inf" if math.isinf(distance) else str(int(distance))

    @staticmethod
    def __to_int(distance: float) -> Optional[int]:
        if math.isinf(distance):
            return None
        return int(distance)
