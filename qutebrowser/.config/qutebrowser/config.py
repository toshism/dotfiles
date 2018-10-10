c.tabs.show = "multiple"
c.tabs.last_close = "close"
c.tabs.tabs_are_windows = True

c.input.insert_mode.auto_load = True
c.input.insert_mode.auto_leave = False

# aliases
c.aliases['pr'] = 'open -t https://stash.uniregistry.com/projects/RAR/repos/uniregistrar/pull-requests'

c.fonts.monospace = "Ubuntu Mono"
c.fonts.statusbar = c.fonts.monospace
c.fonts.tabs = c.fonts.monospace
c.fonts.web.family.sans_serif = c.fonts.monospace
c.fonts.prompts = c.fonts.monospace


# hacky mess to handle switching from hidpi laptop only to mixed with external monitor
import subprocess

# keys
config.bind('sb', 'spawn --userscript org-bookmark')
config.bind('sj', 'spawn --userscript load-bookmark.sh')
