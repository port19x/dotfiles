;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services)
             (gnu home services shells))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (specifications->packages (list "aspell"
                                            "aspell-dict-uk"
                                            "emacs"
                                            "emacs-pdf-tools"
                                            "emacs-vterm"
                                            "fd"
                                            "flameshot"
                                            "font-iosevka"
                                            "git"
                                            "gnupg"
                                            "imagemagick"
                                            "keepassxc"
                                            "mpv"
                                            "openssh"
                                            "pinentry"
                                            "qutebrowser"
                                            "ripgrep"
                                            "slock"
                                            "unzip"
                                            "wget"
                                            "xinit"
                                            "xrandr"
                                            "yt-dlp"
                                            "zstd")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (service home-files-service-type
                  `((".emacs.d/init.el" ,(local-file "./init.el"))
                    (".config/qutebrowser/config.py" ,(local-file "./config.py")))))))
