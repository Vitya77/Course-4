class NetworkSettings:
    """
    This class contains all the settings related to the network and simulation
    """
    NODES_NUMBER_MIN = 2
    ACTION_SLEEP_TIME_SECONDS = 0
    INTERRUPT_ON_ERROR = True
    EXTERNAL_REQUEST_MODE = False
    EXTERNAL_REQUEST_TOTAL_REQUESTS_NUMBER = 0
    NUMBER_OF_REQUESTS = [0]
    WEIGHTS = [1.0]

    INITIATOR_LABEL = 1
    OUTPUT_EVENTS_PATH = "test_results/lab7_events.txt"
    OUTPUT_SUMMARY_PATH = "test_results/lab7_summary.txt"
    OUTPUT_VERIFICATION_PATH = "test_results/lab7_verification.txt"

settings = NetworkSettings()
