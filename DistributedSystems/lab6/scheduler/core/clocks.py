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
