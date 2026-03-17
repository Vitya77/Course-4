from typing import List

from scheduler.core.action import Action


class NodeResponse:
    actions: List[Action]

    def __init__(self, actions: List[Action]):
        self.actions = actions
