local wezterm = require 'wezterm'
return {
  default_prog = { '/usr/bin/zsh' },
  enable_tab_bar = false,
  font = wezterm.font("Iosevka Term", {weight="Regular", stretch="Normal", style="Normal"}),
  font_size = 14.0,
  color_scheme = "Gruvbox dark, hard (base16)",
}
