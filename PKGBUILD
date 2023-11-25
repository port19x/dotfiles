# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r415.51e4947
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://github.com/port19x/dotfiles'
license=('Unlicense')

# sorted by dependency strength: core > program > keybind > alias > emacs-dep > utility
depends=(
'base-devel'
'emacs-nativecomp'
'ttc-iosevka'
'xorg-server'
'xorg-xinit'

'ansible-core'
'keepassxc'
'man-db'
'man-pages'
'mpv'
'noto-fonts'
'zola'

'flameshot'
'libnotify'
'slock'

'yt-dlp'

'fd'
'ripgrep'

'asciiquarium'
'aspell-en'
'entr'
'imagemagick'
'pacman-contrib'
'xorg-xrandr'
)
makedepends=(
'stow'
)

optdepends=(
'brave-bin: webbrowser'
'texlive: for writing papers'
'mullvad-vpn-cli: the best vpn'
'yay: aur helper'
)

source=('dotfiles::git+https://github.com/port19x/dotfiles')
md5sums=('SKIP')

pkgver() {
    cd "$srcdir/${_pkgname}"
    printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)"
}

_manual () {
    printf "\33[2K\r\033[1;33mManual setup: %s\033[0m\n" "$@"
}

package() {
    mkdir -p $HOME/.config/git
    mkdir -p $HOME/.local/share/gnupg
    chmod 700 $HOME/.local/share/gnupg
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[1/3] prepared XDG directories"

    cd .. && stow -v --no-folding --ignore="PKGBUILD" --ignore="src" --ignore="dotfiles" --ignore="pkg" --ignore=".git" -t $HOME .
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[2/3] symlinked config files"

    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[UWU] Starting to compile emacs package. This might take a few minutes" 
    emacs -l ~/.emacs.d/init.el -batch
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[3/3] compiled emacs packages" 

    _manual 'localectl set-x11-keymap de "" "" ctrl:nocaps'
    _manual 'configure autologin: https://wiki.archlinux.org/title/Getty'
    _manual 'git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si'
}
