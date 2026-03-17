import uuid
from typing import Any


class Action:

    action_type: str
    data: dict[Any, Any]
    node_id: uuid.UUID
    action_id: uuid.UUID

    def __init__(self, data: dict[Any: Any], node_id: uuid.UUID, action_id: uuid.UUID) -> None:
        self.data = data
        self.node_id = node_id
        self.action_id = action_id

    def __repr__(self):
        return f"{self.action_type.capitalize()} action for Node {self.node_id} with Data: {self.data}"
