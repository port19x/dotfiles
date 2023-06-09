# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r326.7b0aa7f
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

package() {
    mkdir -p ~/.local/state/zsh && touch ~/.local/state/zsh/history && mkdir -p ~/.cache/zsh/zcompdump-5.9 &&
        printf "\33[2K\r\033[1;32m%s\033[0m\n" "[1/5] prepared zsh history and cache"
    git clone --depth 1 https://github.com/zsh-users/zsh-autosuggestions $HOME/.config/zsh/zsh-autosuggestions &&
        printf "\33[2K\r\033[1;32m%s\033[0m\n" "[2/5] downloaded zsh-autosuggestions" ||
        printf "\33[2K\r\033[1;32m%s\033[0m\n" "[2/5] zsh-autosuggestions already downloaded"
    cd .. && stow -v --no-folding --ignore="PKGBUILD" --ignore="src" --ignore="dotfiles" --ignore="pkg" -t $HOME/.config . && 
        printf "\33[2K\r\033[1;32m%s\033[0m\n" "[3/5] symlinked config files"
    echo "exec awesome" > $HOME/.xinitrc && echo "startx" > $HOME/.config/zsh/.zprofile
        printf "\33[2K\r\033[1;32m%s\033[0m\n" "[4/5] setup awesomewm autostart"
    emacs -l ~/.config/emacs/init.el -batch || true
        printf "\33[2K\r\033[1;32m%s\033[0m\n" "[5/5] compiled emacs packages"
    printf "\33[2K\r\033[1;33mManual setup: %s\033[0m\n" 'echo "export ZDOTDIR=$HOME/.config/zsh" | sudo tee /etc/zsh/zshenv'
    printf "\33[2K\r\033[1;33mManual setup: %s\033[0m\n" 'chsh -s /bin/zsh'
    printf "\33[2K\r\033[1;33mManual setup: %s\033[0m\n" 'git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si'
}
