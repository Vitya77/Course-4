# dss
Distributed systems simulator

The distributed system simulator is a program designed to provide sequential testing of any distributed system and
its algorithms. Its core responsibility is the messaging process between parts (nodes) of a distributed network. 
It can be considered as an observer that collects outbox messages from nodes and, randomly, one by one, sends a message
to a receiving node. Also, an option of adding random external requests to nodes is available, which can be enabled
by setting the EXTERNAL_REQUEST_MODE parameter.
As an abstract system, it is ready for expansion and improvement by manipulating actions while sending them to nodes.
For instance, the following options can be added:
- physical time measuring for an action processing by a node, or any other type of measuring
- Configure the time needed to send a message between nodes (the ACTION_SLEEP_TIME_SECONDS parameter)
- change the data of a message before sending it to the receiver, or even discard sending it
- different types of actions (current system supports only processing of inbox messages),
eg, outbox messages, system messages, etc.

To make the system work with your network implementation, you should follow the next steps:
1. Create your Node class which has to inherit *AbstractNode* class and implement the *process_action* method.
This method should implement the logic of message processing, and return messages to other nodes or nothing. The messages 
should be returned as a *NodeResponse* object
The returning messages will be placed in the mailboxes of the addressee (node) and will be processed in the next turn.
A node instance also has to have a mailbox attribute, which is an instance of the Mailbox class
2. Create your Network class. It has to inherit from the AbstractNetwork class and have the nodes attribute, which is 
a list of all nodes in the network. The implementation of the Network class has to be set to the NETWORK_CLASS parameter in 
the *run_simulation.py* file

