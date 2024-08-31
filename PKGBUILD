# Maintainer: port19 <port19 at port19 dot xyz>
pkgname='port19-dotfiles-git'
_pkgname='dotfiles'
pkgver=r957.eaf7078
pkgrel=1
pkgdesc='My dotfiles package. Superior to an install script.'
arch=('any')
url='https://github.com/port19x/dotfiles'
license=('Unlicense')

# sorted by dependency strength: core > program > languages > emacs-dep > utility
depends=(
'base-devel'
'emacs-nativecomp'
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
'slock'
'qemu-base'
'qemu-ui-gtk'
'zola'

'elixir'
'ghc'
'go'
'julia'
'php'
'python-docs'
'python-matplotlib'
'python-virtualenv'
'roswell'
'quicklisp'
'ruby'
'ruby-docs'
'ruff'
'shellcheck'
'shfmt'
'uv'

'aspell-en'
'cmake'
'fd'
'libvterm'
'poppler-glib'
'ripgrep'

'dfrs'
'fzf'
'imagemagick'
'unzip'
'wget'
'xorg-xrandr'
'yt-dlp'
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
    mkdir -p $HOME/.emacs.d
    cd .. && ln -sf $HOME/dotfiles/init.el $HOME/.emacs.d/init.el
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[1/2] symlinked emacs config, made startup files"

    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[UWU] Starting to compile emacs packages. This might take a few minutes" 
    emacs -l ~/.emacs.d/init.el -batch
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[2/2] compiled emacs packages"

    _manual 'localectl set-x11-keymap de "" "" ctrl:nocaps'
    _manual 'configure autologin: https://wiki.archlinux.org/title/Getty'
    _manual 'git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si'
    _manual 'curl "https://api.github.com/users/port19x/repos?page=1&per_page=100" | grep -e "ssh_url*" | cut -d \" -f 4 | xargs -L1 git clone'
}
