# My Dotfiles

**Purposes**
* Portability of configuration
* Sharing is caring

**Goals**
* Systemwide font configuration
* Systemwide color configuration
* Syntaxbased color consistency through base16

**Non Goals**
* No Transparency
* No Animations
* No Distractions

**Components**
* bash
* kitty
* qutebrowser
    * youtube-dl
    * mpv
* sway
* vim
* waybar

* stow (optional)

**Fonts**
* fonts-font-awesome
* fonts-mononoki

**Quick Setup**

    sudo apt install stow

    stow dots

    sudo apt install $(tail -1 README.md)

    bash kitty qutebrowser youtube-dl mpv sway vim waybar fonts-font-awesome fonts-mononoki
