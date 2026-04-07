import uuid
from typing import List, Dict

from scheduler.abstract.abstract_network import AbstractNetwork
from scheduler.implementation.node import Node


class CurrentNetwork(AbstractNetwork):
    NUMBER_OF_NODES = 5

    def __init__(self) -> None:
        self.nodes = []
        ids = [uuid.UUID(int=index + 1) for index in range(self.NUMBER_OF_NODES)]
        self.__get_edges(ids)
        for node_id in ids:
            self.nodes.append(Node(node_id, self.edges[node_id]))
        super().__init__(self.nodes)

    def __get_edges(self, ids: List[uuid.UUID]) -> Dict[uuid.UUID, List[uuid.UUID]]:
        self.edges = {
            ids[0]: [ids[1], ids[2], ids[3]],
            ids[1]: [ids[0], ids[2]],
            ids[2]: [ids[0], ids[1], ids[3]],
            ids[3]: [ids[0], ids[2], ids[4]],
            ids[4]: [ids[3]],
        }
        return self.edges
