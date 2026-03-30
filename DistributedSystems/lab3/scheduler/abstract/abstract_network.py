from random import choice
from typing import List

from scheduler.abstract.abstract_node import AbstractNode
from scheduler.core.action import Action
from scheduler.core.external_request_generator import ExternalRequestGenerator
from scheduler.settings.network_settings import settings


class AbstractNetwork:
    nodes: List[AbstractNode]
    request_generator: ExternalRequestGenerator

    def __init__(self, nodes: List[AbstractNode]):
        """
        If the external request mode is enabled, it initializes the request generator
        :param nodes:
        """
        if settings.EXTERNAL_REQUEST_MODE:
            self.request_generator = ExternalRequestGenerator(nodes)

    def get_action(self) -> Action:
        """
        The method gathers actions from all nodes mailboxes and returns the random one
        :return: random Action object
        """
        if settings.EXTERNAL_REQUEST_MODE:
            self.process_external_requests()
        actions = [action for node in self.nodes for action in node.mailbox.get_actions()]
        return choice(actions) if actions else None

    def process_external_requests(self) -> None:
        """
        The method obtains external requests and put them into mailboxes of nodes
        :rtype: None
        """
        external_requests = self.request_generator.get_requests()
        for external_request in external_requests:
            node_id, external_request_obj = next(iter(external_request.items()))
            for node in self.nodes:
                if node.node_id == node_id:
                    node.mailbox.add_inbox_action(
                        Action(
                            external_request_obj.to_dict(),
                            node_id=node_id,
                            action_id=external_request_obj.transaction_id
                        )
                    )
