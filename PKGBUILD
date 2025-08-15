# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r1020.7f34ba4
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://github.com/port19x/dotfiles'
license=('Unlicense')

# sorted by dependency strength: core > program > emacs-dep
depends=(
'base-devel'
'emacs'
'noto-fonts'
'noto-fonts-emoji'
'ttc-iosevka'
'xorg-server'
'xorg-xinit'

'flameshot'
'git'
'keepassxc'
'man-db'
'man-pages'
'mpv'
'openssh'
'pulsemixer'
'slock'
'yt-dlp'
'zola'

'aspell-en'
'cmake'
'libvterm'
'poppler-glib'
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
    mkdir -p $HOME/.emacs.d $HOME/.config/keepassxc/ $HOME/.config/mpv/
    echo "[GUI]" > $HOME/.config/keepassxc/keepassxc.ini
    echo "ApplicationTheme=dark" >> $HOME/.config/keepassxc/keepassxc.ini
    echo "demuxer-max-bytes=2GiB" > $HOME/.config/mpv/mpv.conf
    cd ..
    ln -sf $HOME/dotfiles/init.el $HOME/.emacs.d/init.el
    ln -sf $HOME/dotfiles/.gitconfig $HOME/.gitconfig
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[1/2] symlinked emacs and git configs, made startup files"

    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[UWU] Starting to compile emacs packages. This might take a few minutes" 
    emacs -l ~/.emacs.d/init.el -batch
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[2/2] compiled emacs packages"

    _manual 'localectl set-x11-keymap de "" "" ctrl:nocaps'
    _manual 'configure autologin: https://wiki.archlinux.org/title/Getty'
    _manual 'curl "https://api.github.com/users/port19x/repos?page=1&per_page=100" | grep -e "ssh_url*" | cut -d \" -f 4 | xargs -L1 git clone'
}
