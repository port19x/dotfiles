# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r531.c4b7101
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://github.com/port19x/dotfiles'
license=('Unlicense')

# sorted by dependency strength: core > program > emacs-dep > utility > latex
depends=(
'base-devel'
'emacs-nativecomp'
'noto-fonts'
'noto-fonts-emoji'
'qutebrowser'
'ttc-iosevka'
'xorg-server'
'xorg-xinit'

'flameshot'
'git'
'keepassxc'
'man-db'
'man-pages'
'mpv'
'shellcheck'
'shfmt'
'slock'
'zola'

'cmake'
'fd'
'libvterm'
'poppler-glib'
'ripgrep'

'aspell-en'
'dfrs'
'imagemagick'
'tldr'
'unzip'
'wget'
'xorg-xrandr'
'yt-dlp'

'texlive-bibtexextra'
'texlive-binextra'
'texlive-context'
'texlive-fontsextra'
'texlive-fontutils'
'texlive-formatsextra'
'texlive-games'
'texlive-humanities'
'texlive-latexextra'
'texlive-luatex'
'texlive-mathscience'
'texlive-metapost'
'texlive-music'
'texlive-pstricks'
'texlive-publishers'
'texlive-xetex'
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
    echo "startx" > $HOME/.bash_profile
    echo "flameshot &" > $HOME/.xinitrc
    echo "exec emacs --fullscreen" >> $HOME/.xinitrc
    mkdir -p $HOME/.emacs.d $HOME/.config/qutebrowser
    cd .. && ln -sf $HOME/dotfiles/init.el $HOME/.emacs.d/init.el
    cd .. && ln -sf $HOME/dotfiles/config.py $HOME/.config/qutebrowser/config.py
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[1/2] symlinked emacs config, made startup files"

    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[UWU] Starting to compile emacs packages. This might take a few minutes" 
    emacs -l ~/.emacs.d/init.el -batch
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[2/2] compiled emacs packages"

    _manual 'localectl set-x11-keymap de "" "" ctrl:nocaps'
    _manual 'configure autologin: https://wiki.archlinux.org/title/Getty'
    _manual 'git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si'
}
