from typing import List  # noqa: F401

from libqtile import bar, layout, widget
from libqtile.config import Group, Key, Match, Screen
from libqtile.lazy import lazy

mod = "mod4"
terminal = "kitty"

base0 = "#002b36"
base1 = "#073642"
base5 = "#93a1a1"
baseD = "#268bd2"

keys = [
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "space", lazy.layout.next()),
    Key([mod, "shift"], "h", lazy.layout.swap_left()),
    Key([mod, "shift"], "l", lazy.layout.swap_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod], "i", lazy.layout.grow()),
    Key([mod], "m", lazy.layout.shrink()),
    Key([mod], "n", lazy.layout.normalize()),
    Key([mod], "o", lazy.layout.maximize()),
    Key([mod, "shift"], "space", lazy.layout.flip()),

    Key([mod], "Tab", lazy.next_layout()),
    Key([mod], "w", lazy.window.kill()),
    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),
    Key([mod], "r", lazy.spawncmd()),
]

groups = [Group(i) for i in "123"]

for i in groups:
    keys.extend([
        Key([mod], i.name, lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),

        Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            desc="move focused window to group {}".format(i.name)),
    ])

layouts = [
    layout.Max(),
    layout.MonadTall(border_width=1, border_focus=baseD, border_normal=base0),
    layout.MonadWide(border_width=1, border_focus=baseD, border_normal=base0),
]

widget_defaults = dict(
    font='mononoki',
    fontsize=14,
    padding=10,
    background=base0,
    foreground=base5,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(),
    Screen(
        wallpaper="~/Pictures/base00.png",
        top=bar.Bar(
            [
                widget.GroupBox(active=base5, inactive=base5, padding=0),
                widget.Prompt(),
                widget.WindowName(),
                widget.Spacer(),
                widget.CurrentLayout(fmt="{} "),
                # widget.Wttr(format="?T%t", location="redacted"),
                widget.Net(fmt="{} ", format="{down}"),
                widget.Net(fmt="{} ", format="{up}"),
                # widget.CryptoTicker(fmt="{} ", crypto="XMR"),
                # widget.PulseVolume(fmt="{} "),
                widget.CPU(fmt="{}% ", format="{load_percent}"),
                widget.Memory(fmt="{}% ", format="{MemPercent}"),
                widget.DF(fmt="{} ", format="{r:.0f}%", visible_on_warn=False),
                widget.ThermalSensor(fmt="{} ", foreground=base5),
                # widget.Backlight(fmt="{} ", format="{percent:1%}"),
                # widget.Battery(fmt="{} ", format="{percent:1%}"),
                widget.CheckUpdates(distro="Debian", fmt="{} ", display_format="{updates}",\
 no_update_string="0", colour_have_updates=base5, colour_no_updates=base5),
                # widget.ImapWidget(fmt="{} "),
                # widget.KhalCalendar(fmt="{} "),
                widget.Clock(fmt="{} ", format='%a-%d.%m.%y'),
                widget.Clock(fmt="{} ", format='%H:%M'),
            ],
            24,
        ),
    ),
    Screen(),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
])
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
