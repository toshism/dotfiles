c.tabs.show = "multiple"
c.tabs.last_close = "close"
c.tabs.tabs_are_windows = True

c.url.start_pages = "about:blank"

c.input.insert_mode.auto_load = True
c.input.insert_mode.auto_leave = False

config.bind("<Ctrl-f>", "hint links spawn --detach mpv --force-window yes {hint-url}")

# c.fonts.monospace = "Ubuntu Mono"
# c.fonts.statusbar = c.fonts.monospace
# c.fonts.tabs = c.fonts.monospace
# c.fonts.web.family.sans_serif = c.fonts.monospace
# c.fonts.prompts = c.fonts.monospace

c.content.headers.user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36"

c.hints.chars = "asdfjkl;"

# keys
config.bind("sb", "spawn --userscript save-bookmark")
config.bind("sj", "spawn --userscript load-bookmark")

config.bind('<z><l>', 'spawn --userscript qute-pass -d rofi -dmenu')
config.bind('<z><u><l>', 'spawn --userscript qute-pass --username-only')
config.bind('<z><p><l>', 'spawn --userscript qute-pass --password-only')
config.bind('<z><o><l>', 'spawn --userscript qute-pass --otp-only')


# theme
import dracula.draw

# Load existing settings made via :set
config.load_autoconfig()

dracula.draw.blood(c, {"spacing": {"vertical": 6, "horizontal": 8}})

c.qt.workarounds.remove_service_workers = True
