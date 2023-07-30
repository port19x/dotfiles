# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r362.0b841a8
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://github.com/port19x/dotfiles'
license=('Unlicense')

# sorted by dependency strength: core > program > keybind > alias > emacs-dep > utility > latex
depends=(
'awesome'
'base-devel'
'emacs-nativecomp'
'ttc-iosevka'
'wezterm'
'xorg-server'
'xorg-xinit'
'zsh-completions'

'ansible-core'
'htop'
'keepassxc'
'man-db'
'man-pages'
'mpv'
'mupdf'
'noto-fonts'
'pulsemixer'
'zola'

'acpi'
'brightnessctl'
'flameshot'
'libnotify'
'slock'
'songrec'

'bat'
'exa'
'fzf'
'neovim'
'xclip'
'yt-dlp'

'cmake'
'fd'
'ripgrep'

'asciiquarium'
'aspell-en'
'entr'
'imagemagick'
'pacman-contrib'
'xorg-xrandr'

'texlive-bibtexextra'
'texlive-binextra'
'texlive-fontsrecommended'
'texlive-latexextra'
'texlive-plaingeneric'
)
makedepends=(
'stow'
)

optdepends=(
'brave-bin: webbrowser'
'mullvad-vpn-cli: the best vpn'
'signal-desktop: superior messenger'
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
    mkdir -p $HOME/.local/share/gnupg
    chmod 700 $HOME/.local/share/gnupg
}

package() {
    _progress "[1/5] prepared zsh history and cache" mkdir -p ~/.local/state/zsh && touch ~/.local/state/zsh/history && mkdir -p ~/.cache/zsh/zcompdump-5.9
    _progress "[2/5] downloaded zsh-autosuggestions" git clone --depth 1 https://github.com/zsh-users/zsh-autosuggestions $HOME/.config/zsh/zsh-autosuggestions || true
    _progress "[3/5] symlinked config files" cd .. && stow -v --no-folding --ignore="PKGBUILD" --ignore="src" --ignore="dotfiles" --ignore="pkg" -t $HOME/.config .
    _progress "[4/5] setup awesomewm autostart" _writefiles
    _progress "[5/5] compiled emacs packages" emacs -l ~/.config/emacs/init.el -batch
    _manual 'echo "export ZDOTDIR=$HOME/.config/zsh" | sudo tee /etc/zsh/zshenv'
    _manual 'chsh -s /bin/zsh'
    _manual 'localectl set-x11-keymap de "" "" ctrl:nocaps'
    _manual 'configure autologin: https://wiki.archlinux.org/title/Getty'
    _manual 'git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si'
}
