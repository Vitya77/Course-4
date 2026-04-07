import uuid
from typing import Union, Any
from uuid import UUID


class ExternalRequest:
    data: dict[str, {}]
    transaction_id: uuid.UUID

    def __init__(self, data: dict[str, {}], transaction_id) -> None:
        self.data = data
        self.transaction_id = transaction_id

    def to_dict(self) -> dict[str, Union[dict[str, Any], UUID]]:
        self.data.update({'transaction_id': self.transaction_id})
        return self.data
