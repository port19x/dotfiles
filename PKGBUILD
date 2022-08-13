# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r167.bc5d9ed
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://git.sr.ht/~port19/dotfiles'
license=('Unlicense')
depends=(
'acpi'
'awesome'
'bat'
'brightnessctl'
'calcurse'
'dash'
'exa'
'flameshot'
'fzf'
'ghostscript'
'gnupg'
'gutenprint'
'htop'
'keepassxc'
'kitty'
'libnotify'
'man-db'
'man-pages'
'mpv'
'mupdf'
'ncdu'
'neofetch'
'neovim'
'newsboat'
'openssh'
'pacman-contrib'
'pulsemixer'
'qutebrowser'
'ranger'
'slock'
'stow'
'ttc-iosevka'
'tree'
'unzip'
'xclip'
'xorg-server'
'xorg-xinit'
'xorg-xkill'
'yt-dlp'
'zsh'
'zsh-completions'
)
makedepends=(
'git'
)
optdepends=(
'aria2: download accelerator'
'devour: window swallowing'
'jdk11-openjdk: I hate java'
'mullvad-vpn-cli: the best vpn'
'signal-desktop: superior messenger'
'syncplay: watch video together'
'syncthing: share files'
'yay: aur helper'
)
source=('dotfiles::git+https://git.sr.ht/~port19/dotfiles')
md5sums=('SKIP')

pkgver() {
    cd "$srcdir/${_pkgname}"
    printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)"
}

package() {
    cd "$srcdir/${_pkgname}/dots"
    find . -type d -exec mkdir -p -- $HOME/{} \;
    mkdir -p ~/.local/state/zsh
    touch ~/.local/state/zsh/history
    mkdir -p ~/.cache/zsh/zcompdump-5.9
    git clone --depth 1 https://github.com/zsh-users/zsh-autosuggestions $HOME/.config/zsh/zsh-autosuggestions || printf "zsh-autosuggestions already downloaded \n"
    cd ../../..
    stow -v dots
    nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'
    echo 'echo "export ZDOTDIR=$HOME/.config/zsh" | sudo tee /etc/zsh/zshenv' | xclip -selection c
    printf "Finishing command pasted to your clipboard/n"
}
