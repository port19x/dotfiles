# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r1008.5519804
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://github.com/port19x/dotfiles'
license=('Unlicense')

# sorted by dependency strength: core > program > keybind > alias > utility
depends=(
'base-devel'
'foot'
'labwc'
'ttc-iosevka'
'zsh-completions'

'gamemode'
'git'
'keepassxc'
'man-db'
'man-pages'
'mangohud'
'mpv'
'mupdf'
'noto-fonts'
'pulsemixer'
'syncplay'

'swaylock'

'fzf'
'grim'
'neovim'
'slurp'
'wl-clipboard'
'yt-dlp'

'aspell-en'
'dfrs'
'fd'
'imagemagick'
'unzip'
'wget'
)
makedepends=(
'stow'
)

optdepends=(
'brave-bin: webbrowser' #librewolf on alpine
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
    mkdir -p $pkgdir/etc/zsh
    mkdir -p $HOME/.config/git
    mkdir -p $HOME/.local/state/zsh
    mkdir -p $HOME/.cache/zsh/zcompdump-5.9
    mkdir -p $HOME/.local/share/gnupg
    chmod 700 $HOME/.local/share/gnupg
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[1/5] prepared XDG directories"

    touch $HOME/.local/state/zsh/history
    touch $HOME/.config/git/config
    echo "export ZDOTDIR=$HOME/.config/zsh" > $pkgdir/etc/zsh/zshenv
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[2/5] prepared supporting files"

    git clone --depth 1 https://github.com/zsh-users/zsh-autosuggestions $HOME/.config/zsh/zsh-autosuggestions 2>/dev/null || true
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[3/5] downloaded zsh-autosuggestions" #available in alpine as package

    cd .. && stow -v --no-folding --ignore="PKGBUILD" --ignore="src" --ignore="dotfiles" --ignore="pkg" -t $HOME/.config .
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[4/4] symlinked config files"

    _manual 'chsh -s /bin/zsh'
    _manual 'configure autologin: https://wiki.archlinux.org/title/Getty'
    _manual 'git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si'
}
