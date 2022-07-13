#put the following line in your /etc/zsh/zshenv and uncomment it
#export ZDOTDIR=$HOME/.config/zsh
SAVEHIST=1000000
HISTSIZE=$SAVEHIST
setopt INC_APPEND_HISTORY # Don't wait until the shell exits
setopt SHARE_HISTORY # Share history between all sessions

EDITOR='nvim' #git rebases annoy me
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

alias v='nvim'
alias ls='exa'
alias la='exa -a'
alias ll='exa -la'
alias br='brightnessctl set 0 && read && brightnessctl set 100%'
alias pm='pulsemixer'
alias vim='nvim'
alias gts='git status'
alias gta='git add'
alias gtc='git commit -m'
alias gtd='git diff'
alias gtp='git push -u origin master'
alias gtl='git log'
alias yta="yt-dlp --embed-thumbnail -f 'bestaudio/best' -f 'm4a'"
alias ytd="yt-dlp -f 'bestvideo[height<=?1080]+bestaudio/best' -f 'mp4'"
alias ytdd="yt-dlp -f 'bestvideo[height<=?720]+bestaudio/best' -f 'mp4'"
alias ytddd="yt-dlp -f 'bestvideo[height<=?480]+bestaudio/best' -f 'mp4'"
alias stamp='date +%d.%m.%y'
alias icat='kitty +kitten icat'
alias smpv='mpv "$(ls | shuf -n 1)"'
alias fmpv='mpv "$(fzf)"'
alias lofi='mpv --no-vid "https://youtu.be/5qap5aO4i9A"'
alias yank='xclip -selection c < '
alias song='ps "$(pgrep mpv)"'
alias demu='devour mupdf'
alias news='newsboat -x reload && newsboat -x print-unread'
alias rm='rm -I --preserve-root' #Every alias has a story ...
