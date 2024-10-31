export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export EDITOR='nvim'
export MANROFFOPT="-c"
export MANPAGER="sh -c 'col -bx | batcat -l man -p'"
export GNUPGHOME="$HOME/.local/share/gnupg"
export GOPATH="$HOME/.cache/go"
export ANSIBLE_HOME="$HOME/.cache/ansible"
export NO_AT_BRIDGE=1
export SAVEHIST=1000000
export HISTSIZE=$SAVEHIST
set -o vi
source /etc/bash_completion.d/git-prompt
export GIT_PS1_SHOWDIRTYSTATE=1
export PROMPT_COMMAND='PS1_CMD1=$(__git_ps1 " (%s)")'
export PS1='\w${PS1_CMD1}\\$ '

pastebin() {
    curl --silent https://oshi.at -F f=@$* -F expire=120 \
    | grep DL \
    | cut -d " " -f 2 \
    | xclip -selection c \
    && echo "link copied to clipboard"
}

backup() {
    a=$(date +%d.%m.%y)
    du -shc "$HOME/dl" "$HOME/doc" "$HOME/dotfiles" "$HOME/pic" "$HOME/.ssh" "Passwords.kdbx"
    printf "Is the backup small enough? (CTRL-C otherwise)"
    read
    echo "$a: initiating backup"
    cd
    mkdir "$a"
    cp -r "dl" "doc" "dotfiles" "pic" ".ssh" "Passwords.kdbx" "$a"
    tar c -I"zstd -19 -T0" -f "$a.tar.zst" "$a"
    gpg -c "$a.tar.zst"
}

f() {
    "$@" "$(fzf)"
}

alias v='nvim'
alias ap='ansible-playbook'
alias br='brightnessctl set 0 && read && brightnessctl set 100%'
alias ct='cd $(mktemp -d)'
alias la='exa -a'
alias ll='exa -la'
alias ls='exa'
alias pm='pulsemixer'
alias rm='rm -I --preserve-root'
alias cdd='pushd'
alias cat='batcat'
alias dnb='mpv --no-video https://youtube.com/@themanfromdelmonte'
alias gta='git add'
alias gtc='git commit -m'
alias gtcc='git commit --amend --no-edit'
alias gtd='git diff'
alias gtf='git pull'
alias gtl='git log'
alias gtp='git push'
alias gtpp='git push --force-with-lease'
alias gts='git status'
alias yta="yt-dlp --embed-thumbnail -f 'bestaudio/best' -f 'm4a'"
alias ytd="yt-dlp -f 'bestvideo[height<=?1080]+bestaudio/best' -f 'mp4'"
alias icat='wezterm imgcat'
alias lofi='mpv --no-video https://www.youtube.com/live/jfKfPfyJRdk'
alias yank='xclip -selection c < '
alias phonk='mpv --no-video https://www.youtube.com/watch?v=suYOayE3e00'
