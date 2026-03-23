import uuid
from time import sleep
from timeit import default_timer

from scheduler.abstract.abstract_network import AbstractNetwork
from scheduler.abstract.abstract_node import AbstractNode
from scheduler.core.action import Action
from scheduler.settings.network_settings import settings


class Observer:
    """
    A main class which runs simulation.
    """
    network: AbstractNetwork
    nodes: dict[uuid.UUID, AbstractNode]

    def __init__(self, network: AbstractNetwork):
        nodes = network.nodes
        if nodes is None or len(nodes) < settings.NODES_NUMBER_MIN:
            raise ValueError("The observer must have at least two nodes")
        self.network = network
        self.nodes = {node.node_id: node for node in nodes}

    def process_action(self, action: Action):
        if action.action_type == 'inbox':
            print(f"Processing Action ID: {action.action_id}, DATA: {action}")
            start = default_timer()
            response = self.nodes[action.node_id].send(action)
            for incoming_action in response.actions:
                self.nodes[incoming_action.node_id].mailbox.add_inbox_action(incoming_action)
                print(f"New incoming message for node {incoming_action.node_id} and data {incoming_action.data}")
            print(f"Processed a message for node {action.node_id}. Time: {default_timer() - start} seconds\n")
        else:
            raise ValueError("Unknown action type")

    def run(self):
        while True:
            action = self.network.get_action()
            if action:
                print(action)
                try:
                    self.process_action(action)
                except Exception as e:
                    print(f"Cannot process action {action}. Exception: {e}")
                    if settings.INTERRUPT_ON_ERROR:
                        raise e
                else:
                    self.nodes.get(action.node_id).mailbox.remove_action(action)
            else:
                print("No available action found")
            sleep(settings.ACTION_SLEEP_TIME_SECONDS)
