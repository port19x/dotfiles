config.load_autoconfig(True)

c.changelog_after_upgrade = 'patch'
c.colors.webpage.preferred_color_scheme = 'dark'
c.completion.cmd_history_max_items = 0
c.completion.shrink = True
c.completion.timestamp_format = '%H:%M %d.%m'
c.completion.web_history.max_items = 1000
c.confirm_quit = ['downloads']
c.content.autoplay = False
c.content.canvas_reading = False
#c.content.cookies.accept = 'all' #teams need 3rdparty cookies
c.content.cookies.accept = 'no-3rdparty' #teams need 3rdparty cookies
c.content.cookies.store = False
c.content.fullscreen.overlay_timeout = 0
c.content.geolocation = False
c.content.mouse_lock = False
c.content.webrtc_ip_handling_policy = 'default-public-interface-only'
c.downloads.location.prompt = False
c.downloads.remove_finished = 1000
c.input.forward_unbound_keys = 'none'
c.keyhint.delay = 0
c.new_instance_open_target = 'tab-bg-silent'
c.qt.low_end_device_mode = 'never'
c.scrolling.bar = 'never'
c.statusbar.widgets = ['keypress', 'scroll', 'progress']
c.tabs.close_mouse_button = 'none'
c.tabs.indicator.width = 0
c.tabs.last_close = 'default-page'
c.tabs.new_position.related = 'last'
c.tabs.undo_stack_size = 10

c.url.default_page = 'https://start.duckduckgo.com/'
c.url.start_pages = ['https://start.duckduckgo.com']

config.bind(',m', 'hint links spawn kitty -e mpv {hint-url}')
config.bind(',cm', 'spawn kitty -e mpv {url}')
config.bind(',y', 'hint links spawn kitty -e youtube-dl {hint-url}')
config.bind(',cy', 'spawn kitty -e youtube-dl {url}')
config.bind(',a', 'hint links spawn kitty -e youtube-dl -f bestaudio {hint-url}')
config.bind(',ca', 'spawn kitty -e youtube-dl -f bestaudio {url}')
config.bind(',r', 'restart')
config.bind(',xb', 'config-cycle statusbar.show always never')
config.bind(',xt', 'config-cycle tabs.show always never')
config.bind(',xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')
config.bind('Sd', 'bookmark-del')
config.bind('SD', 'quickmark-del')

fontname = "mononoki"
fontsize = "12pt"

c.fonts.default_family = [fontname]
c.fonts.default_size = fontsize
c.fonts.completion.category = 'default_size default_family'
c.fonts.completion.entry = 'default_size default_family'
c.fonts.contextmenu = 'default_size default_family'
c.fonts.debug_console = 'default_size default_family'
c.fonts.downloads = 'default_size default_family'
c.fonts.hints = 'default_size default_family'
c.fonts.keyhint = 'default_size default_family'
c.fonts.messages.error = 'default_size default_family'
c.fonts.messages.info = 'default_size default_family'
c.fonts.messages.warning = 'default_size default_family'
c.fonts.prompts = 'default_size default_family'
c.fonts.statusbar = 'default_size default_family'
c.fonts.tabs.unselected = 'default_size default_family'

config.source('qutewal.py')
