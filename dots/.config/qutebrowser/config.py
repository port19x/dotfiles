#config
config.load_autoconfig(True)
c.completion.cmd_history_max_items = 0
c.completion.shrink = True
c.completion.timestamp_format = '%H:%M %d.%m.'
c.completion.web_history.max_items = 500
c.confirm_quit = ['downloads']
#c.content.canvas_reading = False #whatsapp web needs canvas for file upload
#c.content.cookies.accept = 'all' #teams and zoom need 3rdparty cookies
c.content.cookies.accept = 'no-3rdparty' #teams need 3rdparty cookies
c.content.cookies.store = False #doesn't support url patterns yet :sob:
c.content.fullscreen.overlay_timeout = 0
c.content.webrtc_ip_handling_policy = 'default-public-interface-only'
c.downloads.location.prompt = False
c.downloads.remove_finished = 1000
c.input.forward_unbound_keys = 'none'
c.keyhint.delay = 0
c.new_instance_open_target = 'tab-bg-silent'
c.scrolling.bar = 'never'
c.statusbar.widgets = ['keypress', 'scroll', 'progress']
c.tabs.last_close = 'default-page'
c.zoom.default = "100%"
c.downloads.location.directory = "~/dl"

#searx
c.url.default_page = 'https://search.bus-hit.me/'
c.url.start_pages = 'https://search.bus-hit.me/'
c.url.searchengines = {'DEFAULT':'https://search.bus-hit.me/?q={}'}

#keybinds
config.bind(',m', 'hint links spawn mpv {hint-url}')
config.bind(',nv', 'hint links spawn mpv --no-video {hint-url}')
config.bind(',y', 'hint links spawn kitty -e yt-dlp {hint-url}')
config.bind(',a', 'hint links spawn kitty -e yt-dlp -f bestaudio {hint-url}')
config.bind(',cm', 'spawn mpv {url}')
config.bind(',cnv', 'spawn mpv --no-video {url}')
config.bind(',cy', 'spawn kitty -e yt-dlp {url}')
config.bind(',ca', 'spawn kitty -e yt-dlp -f bestaudio {url}')
config.bind(',r', 'restart')
config.bind(',xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')
config.bind('Sd', 'bookmark-del')
config.bind('SD', 'quickmark-del')

#fonts
c.fonts.default_family = "mononoki"
c.fonts.default_size = "12pt"
c.fonts.contextmenu = 'default_size default_family'
c.fonts.prompts = 'default_size default_family'

#colors
base00 = "#002b36"
base01 = "#073642"
base02 = "#586e75"
base03 = "#657b83"
base04 = "#839496"
base05 = "#93a1a1"
base06 = "#eee8d5"
base07 = "#fdf6e3"
base08 = "#dc322f"
base09 = "#cb4b16"
base0A = "#b58900"
base0B = "#859900"
base0C = "#2aa198"
base0D = "#268bd2"
base0E = "#6c71c4"
base0F = "#d33682"
c.colors.completion.fg = base05
c.colors.completion.odd.bg = base01
c.colors.completion.even.bg = base00
c.colors.completion.category.fg = base0A
c.colors.completion.category.bg = base00
c.colors.completion.category.border.top = base00
c.colors.completion.category.border.bottom = base00
c.colors.completion.item.selected.fg = base05
c.colors.completion.item.selected.bg = base02
c.colors.completion.item.selected.border.top = base02
c.colors.completion.item.selected.border.bottom = base02
c.colors.completion.item.selected.match.fg = base0B
c.colors.completion.match.fg = base0B
c.colors.completion.scrollbar.fg = base05
c.colors.completion.scrollbar.bg = base00
c.colors.contextmenu.disabled.bg = base01
c.colors.contextmenu.disabled.fg = base04
c.colors.contextmenu.menu.bg = base00
c.colors.contextmenu.menu.fg =  base05
c.colors.contextmenu.selected.bg = base02
c.colors.contextmenu.selected.fg = base05
c.colors.downloads.bar.bg = base00
c.colors.downloads.start.fg = base00
c.colors.downloads.start.bg = base0D
c.colors.downloads.stop.fg = base00
c.colors.downloads.stop.bg = base0C
c.colors.downloads.error.fg = base08
c.colors.hints.fg = base00
c.colors.hints.bg = base0A
c.colors.hints.match.fg = base05
c.colors.keyhint.fg = base05
c.colors.keyhint.suffix.fg = base05
c.colors.keyhint.bg = base00
c.colors.messages.error.fg = base00
c.colors.messages.error.bg = base08
c.colors.messages.error.border = base08
c.colors.messages.warning.fg = base00
c.colors.messages.warning.bg = base0E
c.colors.messages.warning.border = base0E
c.colors.messages.info.fg = base05
c.colors.messages.info.bg = base00
c.colors.messages.info.border = base00
c.colors.prompts.fg = base05
c.colors.prompts.border = base00
c.colors.prompts.bg = base00
c.colors.prompts.selected.bg = base02
c.colors.statusbar.normal.fg = base0B
c.colors.statusbar.normal.bg = base00
c.colors.statusbar.insert.fg = base00
c.colors.statusbar.insert.bg = base0D
c.colors.statusbar.passthrough.fg = base00
c.colors.statusbar.passthrough.bg = base0C
c.colors.statusbar.private.fg = base00
c.colors.statusbar.private.bg = base01
c.colors.statusbar.command.fg = base05
c.colors.statusbar.command.bg = base00
c.colors.statusbar.command.private.fg = base05
c.colors.statusbar.command.private.bg = base00
c.colors.statusbar.caret.fg = base00
c.colors.statusbar.caret.bg = base0E
c.colors.statusbar.caret.selection.fg = base00
c.colors.statusbar.caret.selection.bg = base0D
c.colors.statusbar.progress.bg = base0D
c.colors.statusbar.url.fg = base05
c.colors.statusbar.url.error.fg = base08
c.colors.statusbar.url.hover.fg = base05
c.colors.statusbar.url.success.http.fg = base0C
c.colors.statusbar.url.success.https.fg = base0B
c.colors.statusbar.url.warn.fg = base0E
c.colors.tabs.bar.bg = base00
c.colors.tabs.indicator.start = base0D
c.colors.tabs.indicator.stop = base0C
c.colors.tabs.indicator.error = base08
c.colors.tabs.odd.fg = base05
c.colors.tabs.odd.bg = base01
c.colors.tabs.even.fg = base05
c.colors.tabs.even.bg = base00
c.colors.tabs.pinned.even.bg = base0C
c.colors.tabs.pinned.even.fg = base07
c.colors.tabs.pinned.odd.bg = base0B
c.colors.tabs.pinned.odd.fg = base07
c.colors.tabs.pinned.selected.even.bg = base02
c.colors.tabs.pinned.selected.even.fg = base05
c.colors.tabs.pinned.selected.odd.bg = base02
c.colors.tabs.pinned.selected.odd.fg = base05
c.colors.tabs.selected.odd.fg = base05
c.colors.tabs.selected.odd.bg = base02
c.colors.tabs.selected.even.fg = base05
c.colors.tabs.selected.even.bg = base02
