class NetworkSettings:
    """
    This class contains all the settings related to the network and simulation
    """
    NODES_NUMBER_MIN = 2 # Minimum allowed number of nodes
    ACTION_SLEEP_TIME_SECONDS = 1 # Time to sleep between action processing in seconds
    INTERRUPT_ON_ERROR = False # Stop running simulation in case of an error during action processing
    EXTERNAL_REQUEST_MODE = True # Allow sending external requests to random node
    EXTERNAL_REQUEST_TOTAL_REQUESTS_NUMBER = 1 # Limit of created external requests. If none the number is unlimited
    NUMBER_OF_REQUESTS = [0, 1, 2] # List of numbers of requests created in one time
    WEIGHTS = [0.5, 0.25, 0.25] # List of weights (chances) for the NUMBER_OF_REQUESTS value to be chosen.

settings = NetworkSettings()