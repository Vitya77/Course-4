import uuid
from abc import abstractmethod
from collections.abc import Generator
from typing import List

from scheduler.core.action import Action
from scheduler.core.node_response import NodeResponse
from scheduler.core.mailbox import Mailbox


class AbstractNode(Generator):

    neighbors: List
    mailbox:  Mailbox()
    node_id: uuid.UUID

    @abstractmethod
    def process_action(self, message: Action) -> NodeResponse:
        """
        The method for internal processing of an action (transaction, message, etc.)
        :rtype: NodeResponse object with list of outbox actions (messages)
        :param message: inbox message to be processed by a node
        """
        pass

    def send(self, value: Action) -> NodeResponse:
        if value is not None:
            return self.process_action(value)

    def throw(self, typ, val=None, tb=None):
        super().throw(typ, val, tb)
