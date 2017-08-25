from afew.filters.BaseFilter import Filter
from afew.FilterRegistry import register_filter
from afew.NotmuchSettings import get_notmuch_new_query
import subprocess


@register_filter
class NotifyFilter(Filter):
    message = 'Notify on new mail'

    def __init__(self, *args, **kwargs):
        super(NotifyFilter, self).__init__(*args, **kwargs)

    @property
    def query(self):
        return get_notmuch_new_query()

    def notify(self, message, length="5000"):
        subprocess.call(['notify-send', '-t', length, message])

    def handle_message(self, message):
        subject = message.get_header('Subject')
        from_header = message.get_header('From')
        notify_message = "{}\nfrom: {}".format(subject, from_header)
        self.notify(notify_message, length="10000")
