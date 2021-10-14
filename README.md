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
* vim

* stow (optional)

**Fonts**
* fonts-mononoki

**Quick Setup**

    sudo apt install git stow python3-pip

    git clone --recurse-submodules git@github.com:ura43/dotfiles.git

    mkdir -p ~/.config/{$(ls ~/dofiles/dots/.config)}

    stow dots

    sudo apt install python3-cffi xserver-xorg python3-xcffib python3-cairocffi libpangocairo-1.0-0

    sudo pip install qtile
