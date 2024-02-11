c.fonts.default_family = "iosevka"
c.fonts.default_size = "12pt"
c.fonts.contextmenu = "default_size default_family"
c.fonts.prompts = "default_size default_family"

config.load_autoconfig(True)
c.colors.webpage.preferred_color_scheme = "dark"
c.completion.cmd_history_max_items = 0
c.completion.shrink = True
c.completion.timestamp_format = "%H:%M %d.%m"
c.completion.web_history.max_items = 500
c.confirm_quit = ["downloads"]
c.content.canvas_reading = False
c.content.cookies.accept = "no-3rdparty"  # teams need 3rdparty cookies
c.content.cookies.store = True  # doesn't support url patterns yet :sob:
c.content.fullscreen.overlay_timeout = 0
c.content.webrtc_ip_handling_policy = "default-public-interface-only"
c.downloads.location.prompt = False
c.downloads.remove_finished = 1000
c.input.forward_unbound_keys = "none"
c.keyhint.delay = 0
c.new_instance_open_target = "tab-bg-silent"
c.scrolling.bar = "never"
c.statusbar.widgets = ["keypress", "scroll", "progress"]
c.tabs.last_close = "default-page"
c.zoom.default = "100%"
c.downloads.location.directory = "~/dl"

c.url.default_page = "https://ddg.gg"
c.url.start_pages = "https://ddg.gg"
c.url.searchengines = {"DEFAULT": "https://ddg.gg/?q={}"}

config.bind(",m", "hint links spawn mpv {hint-url}")
config.bind(",nv", "hint links spawn mpv --no-video {hint-url}")
config.bind(",y", "hint links spawn yt-dlp -o ~/dl/%(title)s.%(ext)s {hint-url}")
config.bind(",a", "hint links spawn yt-dlp -o ~/dl/%(title)s.%(ext)s -f bestaudio {hint-url}")
config.bind(",cm", "spawn mpv {url}")
config.bind(",cnv", "spawn mpv --no-video {url}")
config.bind(",cy", "spawn yt-dlp -o ~/dl/%(title)s.%(ext)s {url}")
config.bind(",ca", "spawn yt-dlp -o ~/dl/%(title)s.%(ext)s -f bestaudio {url}")
config.bind(",r", "restart")
config.bind(",xx", "config-cycle statusbar.show always never;; config-cycle tabs.show always never")
config.bind("Sd", "bookmark-del")
config.bind("SD", "quickmark-del")
