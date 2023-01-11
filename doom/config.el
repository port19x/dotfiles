(setq doom-font (font-spec :family "iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "iosevka aile" :size 14)
      doom-big-font (font-spec :family "iosevka" :size 28)
      doom-theme 'doom-gruvbox
      doom-gruvbox-dark-variant "hard"
      user-full-name "port19"
      user-mail-address "port19@port19.xyz"
      display-line-numbers-type `relative
      org-directory "~/doc/")

(add-hook 'org-mode-hook (lambda () (delete '("\\.pdf\\'" . default) org-file-apps)
                                    (add-to-list 'org-file-apps '("\\.pdf\\'" . "mupdf %s"))))

(map! :after cider-mode :map clojure-mode-map :n "," #'cider-eval-last-sexp) ;Clojure UX
