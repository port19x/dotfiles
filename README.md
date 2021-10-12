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
* mako
* qutebrowser
    * youtube-dl
    * mpv
* sway
* vim
* waybar
* wofi

* stow (optional)

**Fonts**
* fonts-font-awesome
* fonts-mononoki

**Quick Setup**

    git clone --recurse-submodules git@github.com:ura43/dotfiles.git

    sudo apt install stow

    stow dots

    sudo apt install $(tail -1 README.md)

    bash kitty mako qutebrowser youtube-dl mpv sway vim waybar wofi fonts-font-awesome fonts-mononoki 
