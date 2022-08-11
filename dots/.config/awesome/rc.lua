local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local beautiful = require("beautiful")
local naughty = require("naughty")
local hotkeys_popup = require("awful.hotkeys_popup")

local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local theme = {}
theme.font          = "mononoki 10"
theme.bg_normal     = "#fdf6e3"
theme.bg_focus      = "#eee8d5"
theme.bg_urgent     = "#dc322f"
theme.bg_minimize   = "#586e75"
theme.fg_normal     = "#586e75"
theme.fg_focus      = "#586e75"
theme.fg_urgent     = "#dc322f"
theme.fg_minimize   = "#586e75"
theme.useless_gap   = dpi(4)
theme.border_width  = 2
theme.border_normal = "#002b36"
theme.border_focus  = "#2aa198"
theme.border_marked = "#91231c"
theme.wallpaper = "~/pic/Wallpapers/yellow-liquorstore.jpg"
beautiful.init(theme)

awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.top,
}

awful.screen.connect_for_each_screen(function(s)
    awful.tag({ "1", "2", "3", "4", "5" }, s, awful.layout.layouts[1])
    gears.wallpaper.maximized(beautiful.wallpaper, s, true)
end)

-- {{{ Key bindings
globalkeys = gears.table.join(
    -- Group Awesome
    awful.key({ "Mod4",           }, "k",     	hotkeys_popup.show_help, {description = "show keybindings", group = "awesome"}),
    awful.key({ "Mod4", "Control" }, "q", 	awesome.quit, 		 {description = "quit awesome", group = "awesome"}),
    awful.key({ "Mod4", "Control" }, "r", 	awesome.restart, 	 {description = "reload awesome", group = "awesome"}),
    awful.key({ "Mod4", "Control" }, "l",     	function () awful.spawn.with_shell("brightnessctl set 0% && slock && brightnessctl set 100%") end, {description = "lock screen", group = "awesome"}),

    -- Group Mediakeys
    awful.key({}, "XF86MonBrightnessUp", 	function () awful.spawn("brightnessctl set 10%+") end, {description = "raise brightness", group = "mediakey"}),
    awful.key({}, "XF86MonBrightnessDown", 	function () awful.spawn("brightnessctl set 10%-") end, {description = "lower brightness", group = "mediakey"}),

    -- Group Launcher
    awful.key({ "Mod4" 		  }, "y", 	function () awful.spawn("signal-desktop") end, {description = "signal-desktop", group = "launcher"}),
    awful.key({ "Mod4" 		  }, "g", 	function () awful.spawn("keepassxc") end, {description = "keepassxc", group = "launcher"}),
    awful.key({ "Mod4" 		  }, "m", 	function () awful.spawn("mgba-qt") end, {description = "mgba-qt", group = "launcher"}),
    awful.key({ "Mod4" 		  }, "q", 	function () awful.spawn("qutebrowser") end, {description = "qutebrowser", group = "launcher"}),
    awful.key({ "Mod4",           }, "Return", 	function () awful.spawn("kitty -1") end, {description = "launch terminal", group = "launcher"}),
    awful.key({ "Mod4",           }, "s",      	function () awful.spawn("flameshot gui") end, {description = "take a screenshot", group = "launcher"}),
    awful.key({ "Mod4",           }, "b",      	function () awful.spawn.with_shell("notify-send $(acpi -b | cut -d , -f 2)") end, {description = "show battery percentage", group = "launcher"}),
    awful.key({ "Mod4",           }, "t",      	function () awful.spawn.with_shell("notify-send $(date +%H:%M)") end, {description = "show time", group = "launcher"}),

    -- Group Layout
    awful.key({ "Mod4",           }, "l",     	function () awful.tag.incmwfact( 0.05)          end, {description = "increase master size", group = "layout"}),
    awful.key({ "Mod4",           }, "h",     	function () awful.tag.incmwfact(-0.05)          end, {description = "decrease master size", group = "layout"}),
    awful.key({ "Mod4",           }, "Tab", 	function () awful.layout.inc( 1)                end, {description = "select next layout", group = "layout"}),

    -- Group Tag
    awful.key({ "Mod4" 		  }, "1", 	function () awful.screen.focused().tags[1]:view_only() end, {description = "view tag 1", group = "tag"}),
    awful.key({ "Mod4" 		  }, "2", 	function () awful.screen.focused().tags[2]:view_only() end, {description = "view tag 2", group = "tag"}),
    awful.key({ "Mod4" 		  }, "3", 	function () awful.screen.focused().tags[3]:view_only() end, {description = "view tag 3", group = "tag"}),
    awful.key({ "Mod4" 		  }, "4", 	function () awful.screen.focused().tags[4]:view_only() end, {description = "view tag 4", group = "tag"}),
    awful.key({ "Mod4"		  }, "5", 	function () awful.screen.focused().tags[5]:view_only() end, {description = "view tag 5", group = "tag"}),
    awful.key({ "Mod4", "Shift"   }, "1", 	function () client.focus:move_to_tag(client.focus.screen.tags[1]) end, {description = "move focused client to tag 1", group = "tag"}),
    awful.key({ "Mod4", "Shift"   }, "2", 	function () client.focus:move_to_tag(client.focus.screen.tags[2]) end, {description = "move focused client to tag 2", group = "tag"}),
    awful.key({ "Mod4", "Shift"   }, "3", 	function () client.focus:move_to_tag(client.focus.screen.tags[3]) end, {description = "move focused client to tag 3", group = "tag"}),
    awful.key({ "Mod4", "Shift"   }, "4", 	function () client.focus:move_to_tag(client.focus.screen.tags[4]) end, {description = "move focused client to tag 4", group = "tag"}),
    awful.key({ "Mod4", "Shift"   }, "5", 	function () client.focus:move_to_tag(client.focus.screen.tags[5]) end, {description = "move focused client to tag 5", group = "tag"})
)

-- Group Client
clientkeys = gears.table.join(
    awful.key({ "Mod4", 	  }, "w",     	function (c) c:kill()                         		end, {description = "close", group = "client"}),
    awful.key({ "Mod4",           }, "space", 	function () awful.client.focus.byidx( 1) 		end, {description = "focus next by index", group = "client"}),
    awful.key({ "Mod4",           }, "f", 	function (c) c.fullscreen = not c.fullscreen c:raise() 	end, {description = "toggle fullscreen", group = "client"}),
    awful.key({ "Mod4",           }, "o",      	function (c) c:move_to_screen()               		end, {description = "move to next screen", group = "client"}),
    awful.key({ "Mod4", "Shift"   }, "space", 	function () awful.client.swap.byidx(  1)    		end, {description = "swap with next client by index", group = "client"}),
    awful.key({ "Mod4", "Shift"   }, "f",  	awful.client.floating.toggle                     	   , {description = "toggle floating", group = "client"}),
    awful.key({ "Mod4", "Shift"	  }, "o", 	function () awful.screen.focus_relative( 1) 		end, {description = "focus the next screen", group = "client"})
)


-- applies to floating windows
clientbuttons = gears.table.join(
    awful.button({ 	  }, 1, function (c) c:emit_signal("request::activate", "mouse_click", {raise = true}) end),
    awful.button({ "Mod4" }, 1, function (c) c:emit_signal("request::activate", "mouse_click", {raise = true}) awful.mouse.client.move(c) end),
    awful.button({ "Mod4" }, 3, function (c) c:emit_signal("request::activate", "mouse_click", {raise = true}) awful.mouse.client.resize(c) end)
)

root.keys(globalkeys)

-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "pinentry",
        },
        name = {
          "Event Tester",  -- xev.
        },
      }, properties = { floating = true }},

    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
client.connect_signal("manage", function (c)
    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        awful.placement.no_offscreen(c)
    end
end)
-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)
client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

awful.spawn.with_shell("command -v qutebrowser && pgrep -x qutebrowser || qutebrowser")
awful.spawn.with_shell("command -v kitty && pgrep -x kitty || kitty -1")
awful.spawn.with_shell("command -v newsboat && pgrep -x newsboat || newsboat -x reload")
awful.spawn.with_shell("setxkbmap de")
awful.spawn.with_shell('notify-send "$(checkupdates)"')
