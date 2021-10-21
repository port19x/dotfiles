from typing import List  # noqa: F401

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy

mod = "mod4"
terminal = "kitty"

base00 = "#002b36"
base01 = "#073642"
base05 = "#93a1a1"
base0D = "#268bd2"

keys = [
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(),
        desc="Move window focus to other window"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),

    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),

    Key([mod, "control"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(),
        desc="Spawn a command using a prompt widget"),
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
    layout.MonadTall(border_width=1, border_focus=base0D, border_normal=base00),
    layout.MonadWide(border_width=1, border_focus=base0D, border_normal=base00),
]

widget_defaults = dict(
    font='mononoki',
    fontsize=14,
    padding=10,
    background=base00,
    foreground=base05,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(),
    Screen(
        wallpaper="~/Pictures/base00.png",
        top=bar.Bar(
            [
                widget.GroupBox(active=base05, inactive=base05, padding=0),
                widget.Prompt(),
                widget.WindowName(),
                widget.Spacer(),
                widget.CurrentLayout(fmt="{} "),
#                widget.Wttr(format="?T%t", location="redacted"),
                widget.Net(fmt="{} ", format="{down}"),
                widget.Net(fmt="{} ", format="{up}"),
#                widget.CryptoTicker(fmt="{} ", crypto="XMR", currency="EUR"),
#                widget.PulseVolume(fmt="{} "),
                widget.CPU(fmt="{}% ", format="{load_percent}"),
                widget.Memory(fmt="{}% ", format="{MemPercent}"),
                widget.DF(fmt="{} ", format="{r:.0f}%", visible_on_warn = False),
                widget.ThermalSensor(fmt="{} ", foreground=base05),
#                widget.Backlight(fmt="{} ", format="{percent:1%}"),
#                widget.Battery(fmt="{} ", format="{percent:1%}"),
                widget.CheckUpdates(distro="Debian", fmt="{} ", display_format="{updates}",\
 no_update_string="0", colour_have_updates=base05, colour_no_updates=base05),
#                widget.ImapWidget(fmt="{} "),
#                widget.KhalCalendar(fmt="{} "),
                widget.Clock(fmt="{} ", format='%a-%d.%m.%y'),
                widget.Clock(fmt="{} ", format='%H:%M'),
            ],
            24,
        ),
    ),
    Screen(),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
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
