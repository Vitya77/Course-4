import uuid
from typing import Dict, Iterable, Mapping


class LamportClock:

    def __init__(self, initial_value: int = 0) -> None:
        self._time = initial_value

    def tick(self) -> int:
        self._time += 1
        return self._time

    def update(self, received_time: int) -> int:
        self._time = max(self._time, received_time) + 1
        return self._time

    def snapshot(self) -> int:
        return self._time


class VectorClock:

    def __init__(self, node_id: uuid.UUID, node_ids: Iterable[uuid.UUID]) -> None:
        self._node_key = str(node_id)
        ordered_node_keys = []
        for current_node_id in node_ids:
            node_key = str(current_node_id)
            if node_key not in ordered_node_keys:
                ordered_node_keys.append(node_key)
        if self._node_key not in ordered_node_keys:
            ordered_node_keys.append(self._node_key)
        self._clock = {node_key: 0 for node_key in ordered_node_keys}

    def tick(self) -> Dict[str, int]:
        self._clock[self._node_key] = self._clock.get(self._node_key, 0) + 1
        return self.snapshot()

    def update(self, received_clock: Mapping[str, int]) -> Dict[str, int]:
        for node_key, timestamp in received_clock.items():
            self._clock[node_key] = max(self._clock.get(node_key, 0), timestamp)
        self._clock[self._node_key] = self._clock.get(self._node_key, 0) + 1
        return self.snapshot()

    def snapshot(self) -> Dict[str, int]:
        return dict(self._clock)
