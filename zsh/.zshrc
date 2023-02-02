SAVEHIST=1000000
HISTSIZE=$SAVEHIST
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
    | xclip -selection c \
    && echo "link copied to clipboard"
}

pastebinlong() {
    curl --silent https://oshi.at -F f=@$* \
    | grep DL \
    | cut -d " " -f 2 \
    | xclip -selection c \
    && echo "link copied to clipboard"
}

backup() {
    a=$(date +%d.%m.%y)
    du -shc "$HOME/dl" "$HOME/doc" "$HOME/dotfiles" "$HOME/pic" "Passwords.kdbx"
    printf "Is the backup small enough? (CTRL-C otherwise)"
    read
    echo "$a: initiating backup"
    mkdir "$HOME/$a"
    cp -r "$HOME/dl" "$HOME/doc" "$HOME/dotfiles" "$HOME/pic" "Passwords.kdbx" "$HOME/$a"
    tar c -I"zstd -19 -T0" -f "$HOME/$a.tar.zst" "$HOME/$a"
    gpg -c "$HOME/$a.tar.zst"
}

record() {
    filename="$HOME/vid/$(date +%H:%M_%d-%m-%y)"
    sleep 20
    notify-send "recording start"
    echo "Will be saved to $filename.mp4"
    ffmpeg -hide_banner -nostats -loglevel quiet \
        -f x11grab -i :0.0 \
        -f alsa -i default \
        -c:v libx264 -r 30 \
        -crf 21 -preset faster \
        -pix_fmt yuv420p \
        -maxrate 5000K \
        -bufsize 5000K \
        -c:a aac \
        -b:a 160k "$filename.mkv"
    notify-send "recording end"
    ffmpeg -i "$filename.mkv" \
        -hide_banner -nostats -loglevel quiet \
        -c:v copy \
        -c:a copy \
        -movflags +faststart "$filename.mp4"
    rm "$filename.mkv"
}

thumbnailgen() {
    #don't forget to prerender your icon correctly
    #convert -size 256x256 -background "#242938" Bash-Dark.svg Bash-Dark.png
    convert -size 1280x720 xc:#242938 \
        -gravity center -draw "image over 0,0 256,256 $1" \
        -font iosevka-aile -fill white -pointsize 100 -gravity North -draw "text 0,100 \"$2\"" \
        -font iosevka-aile -fill white -pointsize 55 -gravity South -draw "text 0,100 \"$3\"" \
        out.png
    kitty +kitten icat out.png
    echo "(written to out.png)"
}

yeet() {
    kill $(pgrep $1)
}

alias v='nvim'
alias ls='exa'
alias la='exa -a'
alias ll='exa -la'
alias br='brightnessctl set 0 && read && brightnessctl set 100%'
alias vim='nvim'
alias cat='bat'
alias gts='git status'
alias gta='git add'
alias gtc='git commit -m'
alias gtd='git diff'
alias gtp='git push'
alias gtl='git log'
alias yta="yt-dlp --embed-thumbnail -f 'bestaudio/best' -f 'm4a'"
alias ytd="yt-dlp -f 'bestvideo[height<=?1080]+bestaudio/best' -f 'mp4'"
alias ytdd="yt-dlp -f 'bestvideo[height<=?720]+bestaudio/best' -f 'mp4'"
alias ytddd="yt-dlp -f 'bestvideo[height<=?480]+bestaudio/best' -f 'mp4'"
alias stamp='date +%d.%m.%y'
alias icat='wezterm imgcat'
alias smpv='mpv --no-audio-display "$(ls | shuf -n 1)"'
alias fmpv='mpv --no-audio-display "$(fzf)"'
alias yank='xclip -selection c < '
alias song='ps "$(pgrep mpv)"'
alias news='newsboat -x reload && newsboat -x print-unread'
alias tree='exa -a -I .git --tree'
alias rm='rm -I --preserve-root'
