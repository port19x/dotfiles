typeset -a ealiases
ealiases=()
function abbr() {
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
abbr v='nvim'
abbr s='sudo'
abbr m='man'
abbr sc='shellcheck'
abbr f8='flake8'
abbr la='ls -A'
abbr ll='ls -lha'
abbr br='brightnessctl set 0 && read && brightnessctl set 100%'
abbr pm='pulsemixer'
abbr vim='nvim'
abbr gts='git status'
abbr gta='git add'
abbr gtc='git commit -m'
abbr gtd='git diff'
abbr gtp='git push -u origin main'
abbr gtl='git log'
abbr yta="yt-dlp -f 'bestaudio/best' -f 'm4a'"
abbr ytd="yt-dlp -f 'bestvideo[height<=?1080]+bestaudio/best' -f 'mp4'"
abbr ytdd="yt-dlp -f 'bestvideo[height<=?720]+bestaudio/best' -f 'mp4'"
abbr ytddd="yt-dlp -f 'bestvideo[height<=?480]+bestaudio/best' -f 'mp4'"
abbr icat='kitty +kitten icat'
abbr smpv='mpv "$(ls | shuf | head -n 1)"'
abbr cd1='cd ..'
abbr cd2='cd ../..'
abbr cd3='cd ../../..'
abbr cd4='cd ../../../..'
abbr cd5='cd ../../../../..'
abbr rm='rm -I --preserve-root' #Every alias has a story ...
