;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; If you just installed doom emacs on a new distro, remember to tweak init.el and packages.el
;; Also tweak username and mail if installed on a work machine
(setq user-full-name "port19"
      user-mail-address "port19@port19.xyz.xyz")
(setq doom-font (font-spec :family "Mononoki" :size 16)
      doom-big-font (font-spec :family "Mononoki" :size 24))
(setq display-line-numbers-type 'relative)
(setq doom-theme 'doom-solarized-dark)
;; (setq org-directory "~/doc/")
(setq org-directory "~/doc/")
;; - `map!' for binding new keys (press K on it for docs)
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
