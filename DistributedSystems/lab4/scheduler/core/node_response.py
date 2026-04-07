from typing import List

from DistributedSystems.lab4.scheduler.core.action import Action


class NodeResponse:
    actions: List[Action]

    def __init__(self, actions: List[Action]):
        self.actions = actions
