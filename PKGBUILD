# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r130.23ef2c7
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://git.sr.ht/~port19/dotfiles'
license=('Unlicense')
depends=(
'awesome'
'bat'
'brightnessctl'
'calcurse'
'exa'
'ffmpeg'
'flameshot'
'fzf'
'imagemagick'
'keepassxc'
'kitty'
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
'ttf-mononoki'
'unzip'
'xclip'
'yt-dlp'
'zsh'
)
makedepends=(
'git'
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
