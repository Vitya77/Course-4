from scheduler.core.action import Action


class Mailbox:

    def __init__(self):
        self.inbox = []
        self.outbox = []

    def add_outbox_action(self, action: Action):
        action.action_type = 'outbox'
        self.outbox.append(action)

    def add_inbox_action(self, action: Action):
        action.action_type = 'inbox'
        self.inbox.append(action)

    def get_actions(self):
        actions = []
        if len(self.inbox) > 0:
            actions.append(self.inbox[0])
        if len(self.outbox) > 0:
            actions.append(self.outbox[0])
        return actions

    def remove_action(self, action: Action):
        if action.action_type == 'outbox':
            self.outbox.remove(action)
        elif action.action_type == 'inbox':
            self.inbox.remove(action)
        else:
            raise ValueError('Invalid action type')
