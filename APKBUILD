maintainer "port19 <port19 at port19 dot xyz>"
pkgname "dotfiles"
pkgver 9001
pkgrel 1
pkgdesc "My dotfiles package. Superior to an install script."
arch "all"
url "https://github.com/port19x/dotfiles"
license "Unlicense"

# sorted by dependency strength: core > program > keybind > alias > utility
depends "
foot
labwc
librewolf
font-iosevka
font-noto
zsh-autosuggestions
zsh-completions

git
keepassxc
man-db
man-pages
mpv
mupdf
pulsemixer

swaylock

fzf
neovim
wl-clipboard
yt-dlp

aspell-en
imagemagick
"
makedepends "stow"

source "dotfiles::git+https://github.com/port19x/dotfiles"
md5sums "SKIP"

build () {
	return 0
}

_manual () {
    printf "\33[2K\r\033[1;33mManual setup: %s\033[0m\n" "$@"
}

package() {
    mkdir -p $pkgdir/etc/zsh
    mkdir -p $HOME/.config/git
    mkdir -p $HOME/.local/state/zsh
    mkdir -p $HOME/.cache/zsh/zcompdump-5.9
    mkdir -p $HOME/.local/share/gnupg
    chmod 700 $HOME/.local/share/gnupg
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[1/3] prepared XDG directories"

    touch $HOME/.local/state/zsh/history
    touch $HOME/.config/git/config
    echo "export ZDOTDIR=$HOME/.config/zsh" > $pkgdir/etc/zsh/zshenv
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[2/3] prepared supporting files"

    cd .. && stow -v --no-folding --ignore="PKGBUILD" --ignore="src" --ignore="dotfiles" --ignore="pkg" -t $HOME/.config .
    printf "\33[2K\r\033[1;32m%s\033[0m\n" "[3/3] symlinked config files"

    _manual 'chsh -s /bin/zsh'
    _manual 'configure autologin: https://wiki.archlinux.org/title/Getty'
}
