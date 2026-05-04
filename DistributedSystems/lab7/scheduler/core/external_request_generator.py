import string
import uuid
from datetime import datetime
from random import choices, choice
from typing import List
from uuid import UUID

from scheduler.abstract.abstract_node import AbstractNode
from scheduler.core.external_request import ExternalRequest
from scheduler.settings.network_settings import settings


class ExternalRequestGenerator:

    def __init__(self, nodes: List[AbstractNode]):
        self.nodes = [node for node in nodes if len(node.neighbors) == 1]
        self.total_request_limit = settings.EXTERNAL_REQUEST_TOTAL_REQUESTS_NUMBER

    def get_requests(self) -> list[dict[UUID, ExternalRequest]]:
        number_of_requests = choices(settings.NUMBER_OF_REQUESTS, settings.WEIGHTS)[0]
        requests = []
        while number_of_requests > 0 and (self.total_request_limit is None or self.total_request_limit > 0):
            transaction_id = uuid.uuid4()
            request = ExternalRequest({
                "transaction_id":transaction_id,
                "transaction_data": ''.join(choices(string.ascii_uppercase + string.digits, k=10)),
                "message_type": "New"
            }, transaction_id)
            requests.append({choice(self.nodes).node_id: request})
            number_of_requests -= 1
            if self.total_request_limit is not None:
                self.total_request_limit -= 1
        self.__save_request(requests)
        return requests

    def __save_request(self, requests: List[dict[UUID, ExternalRequest]]) -> None:
        if requests:
            with open('test_results/requests.txt', 'a') as file:
                for request in requests:
                    node_id, external_request_obj = next(iter(request.items()))
                    file.write(f"Transaction ID -- {external_request_obj.transaction_id} -- "
                               f"Node ID -- {node_id} -- "
                               f"Time -- {datetime.now()}" + "\n")
