(setq doom-font (font-spec :family "iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "iosevka aile" :size 14)
      doom-big-font (font-spec :family "iosevka" :size 28)
      doom-theme 'doom-gruvbox
      doom-gruvbox-dark-variant "hard"
      user-full-name "port19"
      user-mail-address "port19@port19.xyz"
      display-line-numbers-type `relative
      org-directory "~/doc/"
      dired-kill-when-opening-new-dired-buffer t)

(add-hook 'org-mode-hook (lambda () (delete '("\\.pdf\\'" . default) org-file-apps)
                                    (add-to-list 'org-file-apps '("\\.pdf\\'" . "mupdf %s"))))

(add-to-list 'display-buffer-alist
                 '(;; no window
                   "\\`\\*Async Shell Command\\*\\'"
                   (display-buffer-no-window)))

(map! :after cider-mode :map clojure-mode-map :n "," #'cider-eval-last-sexp)
(map! :after cider-mode :map clojure-mode-map :n ";" #'cider-format-buffer)
