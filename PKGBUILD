# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r996.ea5a4f9
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://github.com/port19x/dotfiles'
license=('Unlicense')

# sorted by dependency strength: core > program > keybind > alias > utility
depends=(
'awesome'
'base-devel'
'ttc-iosevka'
'wezterm'
'xorg-server'
'xorg-xinit'
'zsh-completions'

'git'
'keepassxc'
'man-db'
'man-pages'
'mpv'
'mupdf'
'noto-fonts'
'pulsemixer'
'zola'

'flameshot'
'libnotify'
'slock'
'songrec'

'bat'
'eza'
'fzf'
'neovim'
'xclip'
'yt-dlp'

'asciiquarium'
'aspell-en'
'dfrs'
'imagemagick'
'pacman-contrib'
'unzip'
'xorg-xrandr'
)
makedepends=(
'stow'
)

optdepends=(
'brave-bin: webbrowser'
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
    mkdir -p $HOME/.config/X11
    mkdir -p $HOME/.config/git
    mkdir -p $HOME/.local/state/zsh
    mkdir -p $HOME/.cache/zsh/zcompdump-5.9
    mkdir -p $HOME/.local/share/gnupg
    chmod 700 $HOME/.local/share/gnupg
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[1/5] prepared XDG directories"

    touch $HOME/.local/state/zsh/history
    touch $HOME/.config/git/config
    echo "exec awesome" > $HOME/.config/X11/xinitrc
    echo "export ZDOTDIR=$HOME/.config/zsh" > $pkgdir/etc/zsh/zshenv
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[2/5] prepared supporting files"

    git clone --depth 1 https://github.com/zsh-users/zsh-autosuggestions $HOME/.config/zsh/zsh-autosuggestions 2>/dev/null || true
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[3/5] downloaded zsh-autosuggestions" 

    cd .. && stow -v --no-folding --ignore="PKGBUILD" --ignore="src" --ignore="dotfiles" --ignore="pkg" -t $HOME/.config .
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[4/4] symlinked config files"

    _manual 'chsh -s /bin/zsh'
    _manual 'localectl set-x11-keymap de "" "" ctrl:nocaps'
    _manual 'configure autologin: https://wiki.archlinux.org/title/Getty'
    _manual 'git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si'
}
