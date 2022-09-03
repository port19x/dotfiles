export SDL_VIDEODRIVER=wayland
export _JAVA_AWT_WM_NONREPARENTING=1
export GDK_BACKEND="wayland,x11"
export MOZ_ENABLE_WAYLAND=1
export EDITOR='nvim'
export QT_QPA_PLATFORM=wayland-egl
export HISTFILE="$HOME/.local/state/zsh/history"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
exec Hyprland
