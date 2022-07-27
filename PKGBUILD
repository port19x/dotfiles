# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r155.2066399
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://git.sr.ht/~port19/dotfiles'
license=('Unlicense')
depends=(
'acpi'
'arch-wiki-lite'
'asciiquarium'
'awesome'
'bat'
'brightnessctl'
'calcurse'
'dash'
'dhcpcd'
'exa'
'expac'
'ffmpeg'
'flameshot'
'fzf'
'ghostscript'
'gnupg'
'gutenprint'
'htop'
'imagemagick'
'iwd'
'keepassxc'
'kitty'
'man-db'
'man-pages'
'mgba-qt'
'mpv'
'mupdf'
'ncdu'
'neofetch'
'neovim'
'newsboat'
'noto-fonts'
'openssh'
'picom'
'pamixer'
'ranger'
'slock'
'stow'
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
'axel: download accelerator'
'devour: window swallowing'
'ffmpeg4.4: undeclared mgba-qt dependency on arm'
'jdk11-openjdk: I hate java'
'mullvad-vpn-cli: the best vpn'
'qutebrowser-qt6-git: webbrowser'
'signal-desktop: superior messenger'
'syncplay: watch video together'
'syncthing: share files'
'ttf-mononoki: primary font'
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
    printf "To finalize the install:	 stow -v dots \n"
    printf "To remove build residue: 	 rm -rf dotfiles *.zst src pkg \n"
}
