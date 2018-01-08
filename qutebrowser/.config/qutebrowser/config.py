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

try:
    screens = int(subprocess.check_output(["xrandr | grep ' connected' | wc -l"], shell=True))
    if screens > 1:
        c.zoom.default = 100
        c.qt.highdpi = False
    else:
        c.zoom.default = 67
        c.qt.highdpi = True
except:
    pass

print("###########")
print (c.zoom.default)
print("###########")
