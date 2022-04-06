;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; If you just installed doom emacs on a new distro, remember to tweak init.el and packages.el
;; Also tweak username and mail if installed on a work machine
(setq user-full-name "ura43"
      user-mail-address "ura@ura43.xyz")
(setq doom-font (font-spec :family "Mononoki" :size 32)
      doom-big-font (font-spec :family "Mononoki" :size 64))
(setq display-line-numbers-type 'relative)
(setq doom-theme 'doom-solarized-dark)
;; (setq org-directory "~/doc/")
(setq org-directory "~/Documents/")
;; - `map!' for binding new keys (press K on it for docs)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(require 'elcord)
(elcord-mode)
