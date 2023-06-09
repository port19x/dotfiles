# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r330.2044c0e
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://github.com/port19x/dotfiles'
license=('Unlicense')

depends=(
'acpi'
'ansible'
'aspell-en'
'awesome'
'bat'
'base-devel'
'brightnessctl'
'clojure'
'cmake'
'emacs-nativecomp'
'exa'
'fd'
'fzf'
'flameshot'
'gnupg'
'htop'
'keepassxc'
'libnotify'
'man-db'
'man-pages'
'mpv'
'mupdf'
'neovim'
'noto-fonts'
'openssh'
'pacman-contrib'
'pulsemixer'
'ripgrep'
'rlwrap'
'slock'
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
'clojure-lsp-bin: lsp for the superior lisp'
'librewolf-bin: webbrowser'
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

_progress () {
    message="$1"
    shift
    $@
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "$message"
}

_manual () {
    printf "\33[2K\r\033[1;33mManual setup: %s\033[0m\n" "$@"
}

_writefiles () {
    echo "exec awesome" > $HOME/.xinitrc
    echo "startx" > $HOME/.config/zsh/.zprofile
}

package() {
    _progress "[1/5] prepared zsh history and cache" mkdir -p ~/.local/state/zsh && touch ~/.local/state/zsh/history && mkdir -p ~/.cache/zsh/zcompdump-5.9
    _progress "[2/5] downloaded zsh-autosuggestions" git clone --depth 1 https://github.com/zsh-users/zsh-autosuggestions $HOME/.config/zsh/zsh-autosuggestions || true
    _progress "[3/5] symlinked config files" cd .. && stow -v --no-folding --ignore="PKGBUILD" --ignore="src" --ignore="dotfiles" --ignore="pkg" -t $HOME/.config .
    _progress "[4/5] setup awesomewm autostart" _writefiles
    _progress "[5/5] compiled emacs packages" emacs -l ~/.config/emacs/init.el -batch
    _manual 'echo "export ZDOTDIR=$HOME/.config/zsh" | sudo tee /etc/zsh/zshenv'
    _manual 'chsh -s /bin/zsh'
    _manual 'git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si'
}
