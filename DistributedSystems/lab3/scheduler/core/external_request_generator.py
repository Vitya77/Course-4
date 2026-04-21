import string
import uuid
from datetime import datetime
from pathlib import Path
from random import choices
from typing import List
from uuid import UUID

from scheduler.abstract.abstract_node import AbstractNode
from scheduler.core.external_request import ExternalRequest
from scheduler.settings.network_settings import settings


class ExternalRequestGenerator:

    def __init__(self, nodes: List[AbstractNode]):
        self.nodes = sorted(nodes, key=lambda node: node.node_id.int)
        self.leaf_nodes = [node for node in self.nodes if len(node.neighbors) == 1]
        self.total_request_limit = settings.EXTERNAL_REQUEST_TOTAL_REQUESTS_NUMBER
        self.output_path = Path("test_results/requests.txt")

    def get_requests(self) -> list[dict[UUID, ExternalRequest]]:
        if self.total_request_limit is not None and self.total_request_limit <= 0:
            return []

        number_of_requests = choices(settings.NUMBER_OF_REQUESTS, settings.WEIGHTS)[0]
        requests = []
        while number_of_requests > 0 and (self.total_request_limit is None or self.total_request_limit > 0):
            transaction_id = uuid.uuid4()
            request_data = {
                "transaction_id": transaction_id,
                "transaction_data": ''.join(choices(string.ascii_uppercase + string.digits, k=10)),
                "message_type": "New"
            }
            if settings.WAVE_ALGORITHM == "tree":
                for node in self.leaf_nodes:
                    requests.append({node.node_id: ExternalRequest(dict(request_data), transaction_id)})
            else:
                requests.append({self.nodes[0].node_id: ExternalRequest(request_data, transaction_id)})
            number_of_requests -= 1
            if self.total_request_limit is not None:
                self.total_request_limit -= 1
        self.__save_request(requests)
        return requests

    def __save_request(self, requests: List[dict[UUID, ExternalRequest]]) -> None:
        if requests:
            self.output_path.parent.mkdir(parents=True, exist_ok=True)
            with self.output_path.open('a', encoding='utf-8') as file:
                for request in requests:
                    node_id, external_request_obj = next(iter(request.items()))
                    file.write(f"Transaction ID -- {external_request_obj.transaction_id} -- "
                               f"Node ID -- {node_id.int} -- "
                               f"Time -- {datetime.now()}" + "\n")

    def has_pending_requests(self) -> bool:
        return self.total_request_limit is None or self.total_request_limit > 0
