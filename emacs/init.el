;;; Bucket List
; https://github.com/KirmTwinty/keyfreq
; read orgmode docs
; vc map on v
;;; >BOOTSTRAPPING< ;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(eval-after-load 'gnutls '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem")) ;; axe from here (in v29)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(eval-when-compile (require 'use-package)) 
(require 'bind-key) ;; too here
(setq use-package-always-ensure t)

;;; >GLOBAL< ;;;
(use-package emacs
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (savehist-mode 1)
  (set-face-attribute 'default nil :font "Iosevka" :height 140)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sqlite . t)
     (shell . t)
     (emacs-lisp . nil)))
  :custom
  (inhibit-startup-message t)
  (make-backup-files nil)
  (custom-file (concat user-emacs-directory "/custom.el"))
  (display-line-numbers-type `relative)
  (native-comp-deferred-compilation nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (indent-tabs-mode nil)
  (ubiquify-buffer-name-style 'forward)
  (visible-bell t)
  (user-full-name "port19")
  (user-mail-address "port19@port19.xyz")
  (org-directory "~/doc/org")
  (org-confirm-babel-evaluate nil))

;;; >DASHBOARD< ;;;
(use-package dashboard
  :preface
  (defun my/dashboard-banner ()
    (setq dashbard-banner-logo-title
          (format "Emacs ready in %s seconds with %d garbage collections."
                  (emacs-init-time) gcs-done)))
  :custom
  (dashboard-startup-banner "~/dotfiles/emacs/avatar.gif")
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-center-content t)
  (dashboard-week-agenda nil)
  (dashboard-items '((recents  . 5) (bookmarks . 5) (projects . 5) agenda))
  :config
  (dashboard-setup-startup-hook)
  :hook
  ((after-init     . dashboard-refresh-buffer)
   (dashboard-mode . my/dashboard-banner)))

;;; >PACKAGES< ;;;
(use-package doom-themes        :config (load-theme 'doom-nord-aurora t)) ;<- look
(use-package doom-modeline      :config (doom-modeline-mode))

(use-package vertico            :custom (vertico-resize t) ;<- completion
                                :config (vertico-mode))
(use-package marginalia         :config (marginalia-mode))
(use-package which-key          :config (which-key-mode)
                                :custom (which-key-max-display-columns 4)
                                        (which-key-sort-order 'which-key-key-order-alpha)
                                        (which-key-sort-uppercase-first nil))
(use-package corfu              :custom (corfu-auto t)
                                :config (global-corfu-mode))

(use-package projectile         :config (projectile-mode +1)) ;<- living in emacs
(use-package helpful            :custom (helpful-max-buffers 3))
(use-package eshell-toggle      :custom (eshell-toggle-size-fraction 4))
(use-package vterm              :custom (vterm-always-compile-module t))
(use-package elfeed             :custom (elfeed-feeds '("https://port19.xyz/rss.xml"))) ;TODO add more feeds
(use-package org-superstar      :hook   (org-mode . org-superstar-mode))
(use-package pdf-tools          :magic  ("%PDF" . pdf-view-mode) ;FIXME
                                :config (pdf-tools-install :no-query))

(use-package evil               :init   (setq evil-want-keybinding nil) ;<- evil keys
                                :config (evil-mode 1)
                                :custom (evil-undo-system 'undo-redo))
(use-package evil-goggles       :config (evil-goggles-mode))
(use-package evil-vimish-fold   :config (global-evil-vimish-fold-mode))
(use-package evil-collection    :config (evil-collection-init))

(use-package clojure-mode       :mode   "\\.edn\\'" "\\.clj?[scx]\\'") ;<- clojure
(use-package cider              :after  (clojure-mode)
                                :custom (cider-repl-pop-to-buffer-on-connect . nil))
(use-package clj-refactor       :after  (clojure-mode)
                                :custom (cljr-project-clean-prompt nil))
(use-package rainbow-delimiters :hook   (prog-mode . rainbow-delimiters-mode))
(use-package smartparens        :hook   (prog-mode . smartparens-mode))
(use-package format-all         :hook   (clojure-mode . format-all-mode))
(use-package paredit            :hook   (clojure-mode . paredit-mode))
(use-package eglot              :hook   (clojure-mode . eglot-ensure))

(use-package magit              :custom (magit-slow-confirm nil)) ;<- other programming related modes
(use-package hl-todo            :config (global-hl-todo-mode))
(use-package git-gutter         :config (global-git-gutter-mode))
(use-package markdown-mode      :mode   "\\.md\\'")
(use-package lua-mode           :mode   "\\.lua\\'")
(use-package ansible            :mode   "\\.ya?ml\\'")

;;; >KEYBINDINGS< ;;;
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer my-leader-keys
    :keymaps '(normal visual insert emacs)
    :prefix "SPC"
    :global-prefix "C-M-SPC")

  (defvar my-help-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "v") #'helpful-variable)
      (define-key map (kbd "f") #'helpful-callable)
      (define-key map (kbd "k") #'helpful-key)
      (define-key map (kbd "m") #'describe-mode)
      (define-key map (kbd "s") #'helpful-symbol)
      (define-key map (kbd "K") #'describe-keymap)
      (define-key map (kbd "p") #'helpful-at-point)
      (define-key map (kbd "M") #'man)
      ;; TODO info interface similar to man
      map))

  (defvar my-buffer-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "b") #'buffer-menu)
      (define-key map (kbd "i") #'ibuffer)
      (define-key map (kbd "k") #'kill-current-buffer)
      (define-key map (kbd "m") #'switch-to-buffer)
      (define-key map (kbd "n") #'next-buffer) ;FIXME ignore * buffers
      (define-key map (kbd "r") #'revert-buffer)
      map))

  (defvar my-window-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "s") #'split-window-below)
      (define-key map (kbd "v") #'split-window-right)
      (define-key map (kbd "c") #'delete-window)
      (define-key map (kbd "b") #'balance-windows)
      (define-key map (kbd "j") #'enlarge-window)
      (define-key map (kbd "l") #'enlarge-window-horizontally)
      (define-key map (kbd "k") #'shrink-window)
      (define-key map (kbd "h") #'shrink-window-horizontally)
      (define-key map (kbd "w") #'evil-window-next)
      map))

  (defvar my-clojure-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "SPC") #'cider-eval-last-sexp)
      (define-key map (kbd "a") #'clojure-add-arity)
      (define-key map (kbd "b") #'cider-eval-buffer)
      (define-key map (kbd "c") #'cider-repl-clear-output)
      (define-key map (kbd "d") #'cider-eval-defun-at-point)
      (define-key map (kbd "e") #'cider-enlighten-mode)
      (define-key map (kbd "f") #'eglot-format)
      (define-key map (kbd "g") #'flymake-show-buffer-diagnostics)
      (define-key map (kbd "h") #'clojure-cycle-privacy)
      (define-key map (kbd "i") #'cider-inspect)
      (define-key map (kbd "I") #'cljr-add-require-to-ns)
      (define-key map (kbd "j") #'cider-jack-in-clj)
      (define-key map (kbd "J") #'cider-jack-in-cljs)
      (define-key map (kbd "k") #'cljr-destructure-keys)
      (define-key map (kbd "l") #'cljr-move-to-let)
      (define-key map (kbd "L") #'cljr-remove-let)
      (define-key map (kbd "m") #'eldoc)
      (define-key map (kbd "n") #'clojure-sort-ns)
      (define-key map (kbd "o") #'eglot-code-actions)
      (define-key map (kbd "p") #'cider-profile-ns-toggle)
      (define-key map (kbd "P") #'cider-profile-summary)
      (define-key map (kbd "q") #'cider-quit)
      (define-key map (kbd "r") #'cljr-rename-symbol)
      (define-key map (kbd "s") #'paredit-forward-slurp-sexp)
      (define-key map (kbd "S") #'paredit-forward-barf-sexp)
      (define-key map (kbd "t") #'clojure-thread)
      (define-key map (kbd "T") #'clojure-thread-last-all)
      (define-key map (kbd "u") #'clojure-unwind)
      (define-key map (kbd "U") #'clojure-unwind-all)
      (define-key map (kbd "v") #'cider-interrupt)
      (define-key map (kbd "w") #'cider-clojuredocs-web)
      (define-key map (kbd "x") #'cljr-extract-function)
      (define-key map (kbd "X") #'cljr-extract-def)
      (define-key map (kbd "y") #'cljr-add-project-dependency)
      (define-key map (kbd "z") #'cljr-update-project-dependencies)
      map))

  (defvar my-org-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "w") #'org-insert-structure-template)
      (define-key map (kbd "e") #'org-babel-execute-src-block)
      (define-key map (kbd "d") #'org-cut-subtree)
      (define-key map (kbd "p") #'org-beamer-export-to-pdf)
      map))

  (defvar my-magic-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "b") #'org-cut-subtree) ; do big font toggle
      (define-key map (kbd "t") #'tetris) ; do big font toggle
      map))

  (my-leader-keys
    "a" '(org-agenda-week-view :which-key "magit clone")
    "b" `(,my-buffer-map :which-key "Buffer")
    "c" '(magit-clone :which-key "magit clone")
    "d" '(dired-jump :which-key "dired jump")
    "e" '(eshell-toggle :which-key "eshell")
    "E" '(vterm-other-window :which-key "vterm")
    "f" '(find-file :which-key "open file")
    "g" `(,my-magic-map :which-key "Magic")
    "h" `(,my-help-map :which-key "Help")
    "i" '(insert-char :which-key "insert unicode")
    "o" `(,my-org-map :which-key "Org Mode")
    "p" `(,project-prefix-map :which-key "Projects")
    "q" '(save-buffers-kill-emacs :which-key "quit emacs")
    "r" '(recentf-open-files :which-key "open recent")
    "s" '(isearch-forward-regexp :which-key "seek")
    "t" '(hl-todo-next :which-key "next Todo")
    "v" '(magit :which-key "magit")
    "w" `(,my-window-map :which-key "Windows")
    "x" '(org-capture :which-key "org capture")
    "SPC" `(,my-clojure-map :which-key "Clojure")
    "<return>" '(bookmark-jump :which-key "jump to bookmark")
    "S-<return>" '(bookmark-set :which-key "set a bookmark")))
