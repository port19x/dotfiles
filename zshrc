SAVEHIST=1000000
HISTSIZE=$SAVEHIST
setopt HIST_IGNORE_SPACE
unsetopt beep
bindkey -v

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
  autoload -Uz compinit
  compinit
fi
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' formats '%F{4}[%F{2}%b%F{4}]%u%c '
zstyle ':vcs_info:*' check-for-changes true
precmd () { vcs_info }
setopt PROMPT_SUBST
PS1='%F{4}%3~ ${vcs_info_msg_0_}%fλ '

alias v='vim'
alias ct='cd $(mktemp -d)'
alias la='ls -a'
alias ll='ls -la'
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
alias dnb='iina https://youtube.com/@themanfromdelmonte'
alias lofi='iina https://www.youtube.com/live/jfKfPfyJRdk'
