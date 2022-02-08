from libqtile import bar, layout, widget
from libqtile.config import Group, Key, Screen, Click, Drag
from libqtile.lazy import lazy

mod = "mod4"
terminal = "kitty -e zsh"

colors = []
cache='/home/ura/.cache/wal/colors'
def load_colors(cache):
    with open(cache, 'r') as file:
        for i in range(8):
            colors.append(file.readline().strip())
    colors.append('#ffffff')
    lazy.reload()
load_colors(cache)


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
    Key([mod], "Return", lazy.spawn(terminal)),
    Key([mod], "r", lazy.spawncmd()),
]

groups = [Group(i) for i in "12345"]

for i in groups:
    keys.extend([
        Key([mod], i.name, lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),

        Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            desc="move focused window to group {}".format(i.name)),
    ])

layouts = [
    layout.Max(),
    layout.MonadTall(border_width=1, border_focus=colors[6], border_normal=colors[0]),
]

widget_defaults = dict(
    font='mononoki',
    fontsize=28,
    padding=10,
    background=colors[0],
    foreground=colors[7],
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(active=colors[7], inactive=colors[7], padding=0),
                widget.Prompt(),
                widget.WindowName(),
                widget.Spacer(),
                widget.CPU(fmt="{}% ", format="{load_percent}"),
                widget.Memory(fmt="{}% ", format="{MemPercent}"),
                widget.CheckUpdates(distro="Arch", fmt="{} ", display_format="{updates}",\
 no_update_string="0", colour_have_updates=colors[7], colour_no_updates=colors[7]),
                widget.Clock(format='%a-%d.%m.%y   %H:%M '),
            ],
            48,
        ),
    )
]
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

follow_mouse_focus = True
wmname = 'Qtile'
float_rules=layout.Floating.default_float_rules
floating_layout = layout.Floating(float_rules=float_rules)
