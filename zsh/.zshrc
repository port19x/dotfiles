SAVEHIST=1000000
HISTSIZE=$SAVEHIST
setopt HIST_IGNORE_SPACE
unsetopt beep
bindkey -v

zstyle :compinstall filename "$HOME/.config/zsh/.zshrc"
autoload -Uz compinit
compinit -d ~/.cache/zsh/zcompdump-5.9
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':completion:*:*:cdr:*:*' menu selection
source ~/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' formats '%F{4}[%F{2}%b%F{4}]%u%c '
zstyle ':vcs_info:*' check-for-changes true
precmd () { vcs_info }
setopt PROMPT_SUBST
PS1='%F{4}%3~ ${vcs_info_msg_0_}%fλ '

pastebin() {
    curl --silent https://oshi.at -F f=@$* -F expire=120 \
    | grep DL \
    | cut -d " " -f 2 \
    | wl-copy \
    && echo "link copied to clipboard"
}

backup() {
    a=$(date +%d.%m.%y)
    du -shc "$HOME/doc" "$HOME/pic" "$HOME/.ssh" "Passwords.kdbx"
    printf "Is the backup small enough? (CTRL-C otherwise)"
    read
    echo "$a: initiating backup"
    cd
    mkdir "$a"
    cp -r "doc" "pic" ".ssh" "Passwords.kdbx" "$a"
    tar c -I"zstd -19 -T0" -f "$a.tar.zst" "$a"
    gpg -c "$a.tar.zst"
}

f() {
    "$@" "$(fzf)"
}

alias v='nvim'
alias ct='cd $(mktemp -d)'
alias la='ls -a'
alias ll='ls -la'
alias pm='pulsemixer'
alias rm='rm -I --preserve-root'
alias dnb='mpv --no-video https://youtube.com/@themanfromdelmonte'
alias gta='git add'
alias gtc='git commit -m'
alias gtcc='git commit --amend --no-edit'
alias gtd='git diff'
alias gtl='git log'
alias gtp='git push'
alias gtpp='git push --force-with-lease'
alias gts='git status'
alias yta="yt-dlp --embed-thumbnail -f 'bestaudio/best' -f 'm4a'"
alias ytd="yt-dlp -f 'bestvideo[height<=?1080]+bestaudio/best' -f 'mp4'"
alias lofi='mpv --no-video https://www.youtube.com/live/jfKfPfyJRdk'
alias yank='wl-copy < '
alias gt='wl-copy ">"'
alias lt='wl-copy "<"'
alias pipe='wl-copy "|"'
