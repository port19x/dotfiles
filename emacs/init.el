(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))

(setq use-package-always-ensure t
      evil-want-keybinding nil
      inhibit-startup-message t
      custom-file (concat user-emacs-directory "/custom.el")
      vertico-resize t
      evil-undo-system 'undo-redo
      lsp-ui-doc-show-with-cursor t
      lsp-ui-doc-delay 2.0
      user-full-name "port19"
      user-mail-address "port19@port19.xyz"
      display-line-numbers-type `relative
      org-directory "~/doc/org"
      dired-kill-when-opening-new-dired-buffer t
      native-comp-deferred-compilation nil
      dashboard-center-content t
      initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(float-time (time-subtract after-init-time before-init-time))

(set-face-attribute 'default nil :font "Iosevka" :height 140)
(use-package better-defaults)
(use-package doom-themes   :init (load-theme 'doom-nord-aurora t))
(use-package vertico       :init (vertico-mode))
(use-package marginalia    :init (marginalia-mode))
(use-package which-key     :init (which-key-mode))
(use-package projectile    :init (projectile-mode +1))
(use-package doom-modeline :init (doom-modeline-mode))
(use-package hl-todo       :init (global-hl-todo-mode))
(use-package git-gutter    :init (global-git-gutter-mode))
(use-package org-superstar :init (add-hook 'org-mode-hook 'org-superstar-mode))
(use-package evil          :init (evil-mode 1))
(use-package evil-goggles     :after evil :init (evil-goggles-mode))
(use-package evil-vimish-fold :after evil :init (evil-vimish-fold-mode))
(use-package evil-collection  :after evil :init (evil-collection-init))
(use-package markdown-mode)
(use-package lua-mode)
(use-package ansible)
(use-package clojure-mode)
  (use-package rainbow-delimiters :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  (use-package smartparens        :init (add-hook 'prog-mode-hook 'smartparens-mode))
  (use-package format-all         :init (add-hook 'clojure-mode-hook 'format-all-mode))
  (use-package paredit            :init (add-hook 'clojure-mode-hook 'paredit-mode))
  (use-package lsp-mode           :init (add-hook 'clojure-mode-hook 'lsp))
  (use-package cider)
  (use-package lsp-ui)
(use-package magit)
(use-package org-re-reveal)
(use-package dashboard
  :preface
  (defun my/dashboard-banner ()
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %s seconds with %d garbage collections."
                  (emacs-init-time) gcs-done)))
  :config
  (setq dashboard-startup-banner "~/dotfiles/emacs/avatar.gif")
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook)
  :hook ((after-init     . dashboard-refresh-buffer)
         (dashboard-mode . my/dashboard-banner)))
