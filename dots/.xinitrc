export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority
xrdb -merge ~/.Xresources
exec qtile start
