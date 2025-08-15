# Bootstrapping

TODO: test procedure in practice and time it

## Booting

- Boot Ventoy USB Stick (F12/F2)
- Select Arch Iso

## Live ISO

```sh
loadkeys de-latin1
iwctl
station wlan0 connect redacted
ping -c1 archlinux.org
archinstall --config-url https://github.com/port19x/dotfiles/blob/master/archinstall.json
```

## Archinstall

- set up disk configuration
- set up user
- verify xorg drivers
- hit install
- exit & reboot

## Postinstall

```sh
git clone --recurse-submodules https://github.com/port19x/dotfiles
cd dotfiles
makepkg -sci
cd brave
git pull
makepkg -sci
startx
```

Restore file backup and you're done

[Backup Script](https://gist.github.com/port19x/8a3160e83d8ebf3b85b84e06aa9ea115)