#Abbreviations
typeset -a ealiases
ealiases=()
function abbrev-alias() {
    alias $1
    ealiases+=(${1%%\=*})
}
function expand-ealias() {
    if [[ $LBUFFER =~ "\<(${(j:|:)ealiases})\$" ]]; then
        zle _expand_alias
        zle expand-word
    fi
    zle magic-space
}
zle -N expand-ealias
bindkey ' '        expand-ealias
bindkey '^ '       magic-space     # control-space to bypass completion
bindkey -M isearch " "      magic-space     # normal space during searches
expand-alias-and-accept-line() {
    expand-ealias
    zle .backward-delete-char
    zle .accept-line
}
zle -N accept-line expand-alias-and-accept-line

#put the following line in your /etc/zsh/zshenv and uncomment it
#export ZDOTDIR=$HOME/.config/zsh
SAVEHIST=1000000
HISTSIZE=$SAVEHIST
setopt INC_APPEND_HISTORY	# Don't wait until the shell exits
setopt SHARE_HISTORY		# Share history between all sessions
setopt HIST_IGNORE_SPACE	# Ignore entries starting with Space
setopt HIST_IGNORE_DUPS		# dup
setopt HIST_IGNORE_ALL_DUPS	# li
setopt HIST_SAVE_NO_DUPS	# cates

PS1='%~$ '
TERM='xterm'
#export MANPAGER='nvim -c 'set ft=man' -'
#mkdir -p ~/.local/state/zsh && touch ~/.local/state/zsh/history
export HISTFILE="$HOME/.local/state/zsh/history"
unsetopt beep
bindkey -v
zstyle :compinstall filename '/home/ks/.config/zsh/.zshrc'
autoload -Uz compinit
#mkdir -p ~/.cache/zsh/zcompdump-5.8
compinit -d ~/.cache/zsh/zcompdump-5.8

#abbreviations
abbrev-alias v='nvim'
abbrev-alias s='sudo'
abbrev-alias m='man'
abbrev-alias sc='shellcheck'
abbrev-alias f8='flake8'
abbrev-alias la='ls -A'
abbrev-alias ll='ls -lha'
abbrev-alias br='brightnessctl set 0 && read && brightnessctl set 100%'
abbrev-alias pm='pulsemixer'
abbrev-alias vim='nvim'
abbrev-alias gts='git status'
abbrev-alias gta='git add'
abbrev-alias gtc='git commit -m'
abbrev-alias gtd='git diff'
abbrev-alias gtp='git push -u origin main'
abbrev-alias gtl='git log'
abbrev-alias yta="yt-dlp -f 'bestaudio/best' -f 'm4a'"
abbrev-alias ytd="yt-dlp -f 'bestvideo[height<=?1080]+bestaudio/best' -f 'mp4'"
abbrev-alias ytdd="yt-dlp -f 'bestvideo[height<=?720]+bestaudio/best' -f 'mp4'"
abbrev-alias ytddd="yt-dlp -f 'bestvideo[height<=?480]+bestaudio/best' -f 'mp4'"
abbrev-alias icat='kitty +kitten icat'
abbrev-alias smpv='mpv "$(ls | shuf | head -n 1)"'
abbrev-alias cd1='cd ..'
abbrev-alias cd2='cd ../..'
abbrev-alias cd3='cd ../../..'
abbrev-alias cd4='cd ../../../..'
abbrev-alias cd5='cd ../../../../..'
abbrev-alias rm='rm -I --preserve-root' #Every alias has a story ...

wal -Rq
