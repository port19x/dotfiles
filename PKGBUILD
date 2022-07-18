# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r137.36fa141
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://git.sr.ht/~port19/dotfiles'
license=('Unlicense')
depends=(
'acpi'
'arch-wiki-lite'
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
'picom'
'pulsemixer'
'qutebrowser'
'rofi'
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
'devour: window swallowing'
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
