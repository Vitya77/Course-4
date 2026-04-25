import uuid
from pathlib import Path
from typing import List, Dict

from scheduler.abstract.abstract_network import AbstractNetwork
from scheduler.core.action import Action
from scheduler.implementation.node import Node
from scheduler.settings.network_settings import settings


class CurrentNetwork(AbstractNetwork):
    NUMBER_OF_NODES = 8

    def __init__(self) -> None:
        self.nodes = []
        ids = [uuid.UUID(int=index + 1) for index in range(self.NUMBER_OF_NODES)]
        self.__get_edges(ids)
        for node_id in ids:
            self.nodes.append(Node(node_id, self.edges[node_id], ids))
        super().__init__(self.nodes)
        self.__prepare_output_files()
        self.__seed_initial_computation(ids)

    def __get_edges(self, ids: List[uuid.UUID]) -> Dict[uuid.UUID, List[uuid.UUID]]:
        self.edges = {
            ids[0]: [ids[1], ids[2]],
            ids[1]: [ids[0], ids[3], ids[4]],
            ids[2]: [ids[0], ids[5], ids[6], ids[7]],
            ids[3]: [ids[1]],
            ids[4]: [ids[1]],
            ids[5]: [ids[2]],
            ids[6]: [ids[2]],
            ids[7]: [ids[2]]
        }
        return self.edges

    def __prepare_output_files(self) -> None:
        results_dir = Path("test_results")
        results_dir.mkdir(parents=True, exist_ok=True)
        (results_dir / "lab6_events.txt").write_text("", encoding="utf-8")
        (results_dir / "lab6_summary.txt").write_text("", encoding="utf-8")

    def __seed_initial_computation(self, ids: List[uuid.UUID]) -> None:
        for initiator_label in settings.INITIAL_INITIATORS:
            initiator_id = ids[initiator_label - 1]
            payload = {
                "message_type": Node.BASE_START,
                "transaction_id": uuid.uuid4(),
                "remaining_hops": settings.INITIAL_WORK_DEPTH,
                "work_origin": initiator_label,
            }
            for node in self.nodes:
                if node.node_id == initiator_id:
                    node.mailbox.add_inbox_action(
                        Action(payload, node_id=initiator_id, action_id=uuid.uuid4())
                    )
                    break

    def is_finished(self) -> bool:
        return all(getattr(node, "termination_announced", False) for node in self.nodes)
