# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r239.c3615f3
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://github.com/port19x/dotfiles'
license=('Unlicense')

depends=(
'acpi'
'aria2'
'aspell-en'
'awesome'
'bat'
'brightnessctl'
'clojure'
'emacs-nativecomp'
'exa'
'fd'
'fzf'
'flameshot'
'gnupg'
'htop'
'i3lock'
'keepassxc'
'libnotify'
'man-db'
'man-pages'
'mpv'
'mupdf'
'ncdu'
'neovim'
'newsboat'
'noto-fonts'
'openssh'
'pacman-contrib'
'pulsemixer'
'qutebrowser'
'ripgrep'
'rofi'
'songrec'
'ttc-iosevka'
'ttc-iosevka-aile'
'wezterm'
'xclip'
'xcolor'
'xorg-server'
'xorg-xinit'
'yt-dlp'
'zola'
'zsh-completions'
)
makedepends=(
'git'
'stow'
)

optdepends=(
'mullvad-vpn-cli: the best vpn'
'signal-desktop: superior messenger'
'tuxedo-keyboard: linux ootb <3'
'yay: aur helper'
)

source=('dotfiles::git+https://github.com/port19x/dotfiles')
md5sums=('SKIP')

pkgver() {
    cd "$srcdir/${_pkgname}"
    printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)"
}

package() {
    mkdir -p ~/.local/state/zsh && touch ~/.local/state/zsh/history && mkdir -p ~/.cache/zsh/zcompdump-5.9 &&
        printf "\33[2K\r\033[1;31m%s\033[0m\n" "[1/6] prepared zsh history and cache"
    git clone --depth 1 https://github.com/zsh-users/zsh-autosuggestions $HOME/.config/zsh/zsh-autosuggestions 2> /dev/null &&
        printf "\33[2K\r\033[1;31m%s\033[0m\n" "[2/6] downloaded zsh-autosuggestions" ||
        printf "\33[2K\r\033[1;31m%s\033[0m\n" "[2/6] zsh-autosuggestions already downloaded"
    echo "exec awesome" > $HOME/.xinitrc &&
        printf "\33[2K\r\033[1;31m%s\033[0m\n" "[3/6] setup awesomewm autostart"
    cd .. && stow -v --no-folding --ignore="PKGBUILD" --ignore="src" --ignore="dotfiles" --ignore="pkg" -t $HOME/.config . && 
        printf "\33[2K\r\033[1;31m%s\033[0m\n" "[4/6] symlinked config files"
    git clone --depth 1 https://github.com/doomemacs/doomemacs $HOME/.config/emacs 2> /dev/null && 
        printf "\33[2K\r\033[1;31m%s\033[0m\n" "[5/6] downloaded doom emacs" ||
        printf "\33[2K\r\033[1;31m%s\033[0m\n" "[5/6] doom emacs already downloaded"
    $HOME/.config/emacs/bin/doom install --fonts --env -! > /dev/null && 
        printf "\33[2K\r\033[1;31m%s\033[0m\n" "[6/6] installed doom emacs"
    printf "\33[2K\r\033[1;31mFinishing command: %s\033[0m\n" 'echo "export ZDOTDIR=$HOME/.config/zsh" | sudo tee /etc/zsh/zshenv'
}
