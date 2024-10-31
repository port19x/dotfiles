# Dotfiles

I'm currently trying out a more mainstream config on Linux Mint.

## Extra Repos

- [Brave](https://brave.com/linux/#debian-ubuntu-mint)
- [Wezterm](https://wezfurlong.org/wezterm/install/linux.html#__tabbed_1_3)

## Extra Packages

```
bat
brave-browser
eza
fd-find
flameshot
fzf
git
imagemagick
keepassxc
mpv
neovim
ripgrep
wezterm-nightly
xclip
```

## How to keep track of the packages

```
comm -23 <(apt-mark showmanual | sort -u) <(gzip -dc /var/log/installer/initial-status.gz | sed -n 's/^Package: //p' | sort -u)
```

[Source](https://askubuntu.com/questions/2389/how-to-list-manually-installed-packages)
