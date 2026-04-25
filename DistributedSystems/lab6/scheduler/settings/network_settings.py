import os


class NetworkSettings:
    """
    This class contains all the settings related to the network and simulation
    """
    TERMINATION_ALGORITHM = os.getenv("LAB6_TERMINATION_ALGORITHM", "rana")  # Supported values: "rana", "safra"
    NODES_NUMBER_MIN = 2  # Minimum allowed number of nodes
    ACTION_SLEEP_TIME_SECONDS = 0  # Time to sleep between action processing in seconds
    INTERRUPT_ON_ERROR = True  # Stop running simulation in case of an error during action processing
    EXTERNAL_REQUEST_MODE = False  # Lab 6 uses a deterministic built-in scenario
    EXTERNAL_REQUEST_TOTAL_REQUESTS_NUMBER = 0
    NUMBER_OF_REQUESTS = [0]
    WEIGHTS = [1.0]

    INITIAL_INITIATORS = [1, 7]
    INITIAL_WORK_DEPTH = 3
    MAX_FORWARD_PER_STEP = 2

settings = NetworkSettings()
