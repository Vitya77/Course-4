import uuid
from typing import Dict, List

from scheduler.abstract.abstract_network import AbstractNetwork
from scheduler.implementation.graph_utils import is_neighborhood_correct, is_tree, is_undirected
from scheduler.implementation.node import Node
from scheduler.settings.network_settings import settings


class CurrentNetwork(AbstractNetwork):
    ECHO_NEIGHBORHOOD = {
        1: {2, 3, 4},
        2: {1, 3},
        3: {1, 2, 4},
        4: {1, 3, 5},
        5: {4},
    }
    TREE_NEIGHBORHOOD = {
        1: {2, 3},
        2: {1, 4, 5},
        3: {1, 6, 7, 8},
        4: {2},
        5: {2},
        6: {3},
        7: {3},
        8: {3},
    }

    def __init__(self) -> None:
        self.nodes = []
        self.neighborhood = self.__select_neighborhood()
        ids = {
            node_number: uuid.UUID(int=node_number)
            for node_number in sorted(self.neighborhood)
        }
        self.__get_edges(ids)
        for node_number in sorted(ids):
            node_id = ids[node_number]
            self.nodes.append(Node(node_id, self.edges[node_id]))
        super().__init__(self.nodes)

    def __select_neighborhood(self) -> Dict[int, set[int]]:
        if settings.WAVE_ALGORITHM == Node.ECHO:
            neighborhood = self.ECHO_NEIGHBORHOOD
        elif settings.WAVE_ALGORITHM == Node.TREE:
            neighborhood = self.TREE_NEIGHBORHOOD
        else:
            raise ValueError(f"Unsupported wave algorithm: {settings.WAVE_ALGORITHM}")

        if not is_neighborhood_correct(neighborhood):
            raise ValueError("The selected communication graph is malformed")
        if not is_undirected(neighborhood):
            raise ValueError("Wave algorithms require an undirected communication graph")
        if settings.WAVE_ALGORITHM == Node.TREE and not is_tree(neighborhood):
            raise ValueError("The tree-wave algorithm requires a tree topology")

        return neighborhood

    def __get_edges(self, ids: Dict[int, uuid.UUID]) -> Dict[uuid.UUID, List[uuid.UUID]]:
        self.edges = {
            ids[node_number]: [ids[neighbor] for neighbor in sorted(neighbors)]
            for node_number, neighbors in self.neighborhood.items()
        }
        return self.edges
