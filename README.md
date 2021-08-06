# My Dotfiles

**Purposes**
* Portability of configuration
* Sharing is caring

**Goals**
* Systemwide font configuration
* Systemwide color configuration
* Syntaxbased color consistency through base16
* Auxiliary info through widgets on the panel

**Non Goals**
* No Transparency
* No Animations
* No Distractions
* Nothing Distrospecific

**Components**
* bash
* kitty
* mononoki
* qutebrowser
    * youtube-dl
    * mpv
* ranger
* sway
* vim
* waybar
    * otf-font-awesome
* wofi

**Copy Pasta**

* Debian

    sudo apt install $(tail -1 README.md)

* Fedora

    sudo dnf install $(tail -1 README.md)

* Arch

    sudo pacman -S $(tail -1 README.md)

* List

    bash kitty mononoki qutebrowser ranger sway vim waybar wofi youtube-dl mpv otf-font-awesome
