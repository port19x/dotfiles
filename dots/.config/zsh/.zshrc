#put the following line in your /etc/zsh/zshenv and uncomment it
#export ZDOTDIR=$HOME/.config/zsh
SAVEHIST=1000000
HISTSIZE=$SAVEHIST
setopt INC_APPEND_HISTORY # Don't wait until the shell exits
setopt SHARE_HISTORY # Share history between all sessions
setopt HIST_IGNORE_SPACE # Ignore entries starting with Space
setopt HIST_IGNORE_DUPS # dup
setopt HIST_IGNORE_ALL_DUPS # li
setopt HIST_SAVE_NO_DUPS # cates

TERM='xterm'
EDITOR='vim' #git rebases annoy me
#mkdir -p ~/.local/state/zsh && touch ~/.local/state/zsh/history
export HISTFILE="$HOME/.local/state/zsh/history"
unsetopt beep
bindkey -v
zstyle :compinstall filename "$HOME/.config/zsh/.zshrc"
autoload -Uz compinit
#mkdir -p ~/.cache/zsh/zcompdump-5.8
compinit -d ~/.cache/zsh/zcompdump-5.8
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':completion:*:*:cdr:*:*' menu selection

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' formats '%F{4}[%F{2}%b%F{4}]%u%c '
zstyle ':vcs_info:*' check-for-changes true
precmd () { vcs_info }
setopt PROMPT_SUBST
PS1='%F{4}%3~ ${vcs_info_msg_0_}%f$ '

#https://github.com/zsh-users/zsh-autosuggestions
source ~/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

alias la='ls -A'
alias ll='ls -lhA'
alias br='brightnessctl set 0 && read && brightnessctl set 100%'
alias pm='pulsemixer'
alias fp='devour mupdf "$(fzf)"' #devour probably isn't installed on debian
alias gts='git status'
alias gta='git add'
alias gtc='git commit -m'
alias gtd='git diff'
alias gtp='git push -u origin master'
alias gtl='git log'
alias yta="yt-dlp -f 'bestaudio/best' -f 'm4a'"
alias ytd="yt-dlp -f 'bestvideo[height<=?1080]+bestaudio/best' -f 'mp4'"
alias ytdd="yt-dlp -f 'bestvideo[height<=?720]+bestaudio/best' -f 'mp4'"
alias ytddd="yt-dlp -f 'bestvideo[height<=?480]+bestaudio/best' -f 'mp4'"
alias icat='kitty +kitten icat'
alias smpv='mpv "$(ls | shuf -n 1)"'
alias fmpv='mpv "$(fzf)"'
alias yank='xclip -selection c < '
alias song='ps "$(pgrep mpv)"'
alias demu='devour mupdf'
alias cd1='cd ..'
alias cd2='cd ../..'
alias cd3='cd ../../..'
alias cd4='cd ../../../..'
alias cd5='cd ../../../../..'
alias rm='rm -I --preserve-root' #Every alias has a story ...
