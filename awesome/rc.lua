-- Boilerplate
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")
menubar.utils.terminal = "wezterm"
menubar.show_categories = false
menubar.utils.lookup_icon = function() end
local hotkeys_popup = require("awful.hotkeys_popup")

-- Themeing
local theme = {}
theme.font          = "iosevka 10"
theme.bg_normal     = "#2e3440" --base00
theme.bg_minimize   = "#434c5e" --base02
theme.fg_normal     = "#8fbcbb" --base07
theme.useless_gap   = 0
theme.border_width  = 1
theme.border_normal = "#2e3440" --base00
theme.border_focus  = "#a3be8c" --base0D
theme.wallpaper = "~/pic/Wallpapers/wallpaper.jpg"
beautiful.init(theme)

-- Layouts
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.top}

-- Tags
awful.screen.connect_for_each_screen(function(s)
    awful.tag({ "1", "2", "3" }, s, awful.layout.layouts[1])
    gears.wallpaper.maximized(beautiful.wallpaper, s, true) end)

-- Key bindings
globalkeys = gears.table.join(
    awful.key({ "Mod4",           }, "k",       hotkeys_popup.show_help, {description = "show keybindings", group = "awesome"}),
    awful.key({ "Mod4", "Control" }, "q",       awesome.quit,            {description = "quit awesome", group = "awesome"}),
    awful.key({ "Mod4", "Control" }, "r",       awesome.restart,         {description = "reload awesome", group = "awesome"}),
    awful.key({ "Mod4", "Control" }, "l",       function () awful.spawn.with_shell("brightnessctl set 0% && slock && brightnessctl set 100%") end, {description = "lock screen", group = "awesome"}),
    awful.key({}, "XF86MonBrightnessUp",        function () awful.spawn("brightnessctl set 10%+") end, {description = "raise brightness", group = "mediakey"}),
    awful.key({}, "XF86MonBrightnessDown",      function () awful.spawn("brightnessctl set 10%-") end, {description = "lower brightness", group = "mediakey"}),
    awful.key({ "Mod4"            }, "r",       function () menubar.show() end, {description = "runlauncher", group = "launcher"}),
    awful.key({ "Mod4"            }, "y",       function () awful.spawn("signal-desktop") end, {description = "signal-desktop", group = "launcher"}),
    awful.key({ "Mod4"            }, "e",       function () awful.spawn("emacsclient -c") end, {description = "emacs", group = "launcher"}),
    awful.key({ "Mod4"            }, "g",       function () awful.spawn("keepassxc") end, {description = "keepassxc", group = "launcher"}),
    awful.key({ "Mod4"            }, "q",       function () awful.spawn("brave") end, {description = "brave", group = "launcher"}),
    awful.key({ "Mod4",           }, "Return",  function () awful.spawn("wezterm") end, {description = "launch terminal", group = "launcher"}),
    awful.key({ "Mod4",           }, "s",       function () awful.spawn("flameshot gui") end, {description = "take a screenshot", group = "launcher"}),
    awful.key({ "Mod4"            }, "m",       function () awful.spawn.with_shell('notify-send "$(timeout 10 songrec recognize -d default || echo song not found)"') end, {description = "songrec", group = "launcher"}),
    awful.key({ "Mod4",           }, "b",       function () awful.spawn.with_shell("notify-send $(acpi -b | cut -d , -f 2)") end, {description = "show battery percentage", group = "launcher"}),
    awful.key({ "Mod4",           }, "t",       function () awful.spawn.with_shell("notify-send $(date +%H:%M_%d.%m.%y)") end, {description = "show time", group = "launcher"}),
    awful.key({ "Mod4",           }, "l",       function () awful.tag.incmwfact( 0.05)          end, {description = "increase master size", group = "layout"}),
    awful.key({ "Mod4",           }, "h",       function () awful.tag.incmwfact(-0.05)          end, {description = "decrease master size", group = "layout"}),
    awful.key({ "Mod4",           }, "Tab",     function () awful.layout.inc( 1)                end, {description = "select next layout", group = "layout"}),
    awful.key({ "Mod4"            }, "1",       function () awful.screen.focused().tags[1]:view_only() end, {description = "view tag 1", group = "tag"}),
    awful.key({ "Mod4"            }, "2",       function () awful.screen.focused().tags[2]:view_only() end, {description = "view tag 2", group = "tag"}),
    awful.key({ "Mod4"            }, "3",       function () awful.screen.focused().tags[3]:view_only() end, {description = "view tag 3", group = "tag"}),
    awful.key({ "Mod4", "Shift"   }, "1",       function () client.focus:move_to_tag(client.focus.screen.tags[1]) end, {description = "move focused client to tag 1", group = "tag"}),
    awful.key({ "Mod4", "Shift"   }, "2",       function () client.focus:move_to_tag(client.focus.screen.tags[2]) end, {description = "move focused client to tag 2", group = "tag"}),
    awful.key({ "Mod4", "Shift"   }, "3",       function () client.focus:move_to_tag(client.focus.screen.tags[3]) end, {description = "move focused client to tag 3", group = "tag"}),
clientkeys = gears.table.join(
    awful.key({ "Mod4",           }, "w",       function (c) c:kill()                                   end, {description = "close", group = "client"}),
    awful.key({ "Mod4",           }, "space",   function () awful.client.focus.byidx( 1)                end, {description = "focus next by index", group = "client"}),
    awful.key({ "Mod4",           }, "f",       function (c) c.fullscreen = not c.fullscreen c:raise()  end, {description = "toggle fullscreen", group = "client"}),
    awful.key({ "Mod4",           }, "o",       function (c) c:move_to_screen()                         end, {description = "move to next screen", group = "client"}),
    awful.key({ "Mod4", "Shift"   }, "space",   function () awful.client.swap.byidx(  1)                end, {description = "swap with next client by index", group = "client"}),
    awful.key({ "Mod4", "Shift"   }, "f",       awful.client.floating.toggle                               , {description = "toggle floating", group = "client"}),
    awful.key({ "Mod4", "Shift"   }, "o",       function () awful.screen.focus_relative( 1)             end, {description = "focus the next screen", group = "client"}))
clientbuttons = gears.table.join(
    awful.button({        }, 1, function (c) c:emit_signal("request::activate", "mouse_click", {raise = true}) end),
    awful.button({ "Mod4" }, 1, function (c) c:emit_signal("request::activate", "mouse_click", {raise = true}) awful.mouse.client.move(c) end),
    awful.button({ "Mod4" }, 3, function (c) c:emit_signal("request::activate", "mouse_click", {raise = true}) awful.mouse.client.resize(c) end))
root.keys(globalkeys)

-- Boilerplate so windows behave
awful.rules.rules = {{ rule = { },
                       properties = { border_width = beautiful.border_width,
                                      border_color = beautiful.border_normal,
                                      focus = awful.client.focus.filter,
                                      raise = true,
                                      size_hints_honor = false,
                                      keys = clientkeys,
                                      buttons = clientbuttons,
                                      screen = awful.screen.preferred,
                                      placement = awful.placement.no_overlap+awful.placement.no_offscreen}}}

-- Follow Focus
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)
client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Autostart
awful.spawn.with_shell("command -v emacs && pgrep -x emacs || emacs --daemon")
awful.spawn.with_shell('notify-send "$(checkupdates)"')
awful.spawn.with_shell("command -v brave && pgrep -x brave || brave")
