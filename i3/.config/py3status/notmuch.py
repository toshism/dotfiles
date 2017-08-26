# -*- coding: utf-8 -*-
"""
Example module that says 'Hello World!'

This demonstrates how to produce a simple custom module.
"""

import subprocess


class Py3status:

    search = "tag:unread"
    format = "✉ {count}"
    refresh = 60

    def notmuch(self):
        count = subprocess.check_output(["notmuch", "count", self.search]).strip()
        return {
            'full_text': self.py3.safe_format(self.format, {'count': count}),
            'cached_until': self.py3.time_in(seconds=self.refresh)
        }
