(setq gc-cons-threshold most-positive-fixnum)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(eval-after-load 'gnutls '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem")) ;; axe from here (in v29)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(eval-when-compile (require 'use-package)) 
(require 'bind-key) ;; too here
(setq use-package-always-ensure t)

(use-package emacs
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (savehist-mode 1)
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
  (sort-fold-case t)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  :hook
  (prog-mode . electric-pair-mode)
  (server-after-make-frame . (lambda () (set-face-attribute 'default nil :font "Iosevka" :height 140)))
  (after-init . (lambda () (setq gc-cons-threshold (* 8 1024 1024))))
  (after-init . elfeed-update))

(use-package org-contrib
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sqlite . t)
     (shell . t)
     (emacs-lisp . nil)))
  :custom
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  (org-directory "~/doc/org")
  (org-default-notes-file "~/doc/org/notes.org")
  (org-agenda-files '("~/doc/org"))
  (org-capture-templates '(("c" "Coaching / Emacs todo" checkitem (file ""))))
  (org-confirm-babel-evaluate nil))

(use-package dashboard
  :custom
  (dashboard-startup-banner "~/dotfiles/emacs/avatar.gif")
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-center-content t)
  (dashboard-items '((recents  . 5) (bookmarks . 5) (projects . 5)))
  :hook
  (server-after-make-frame     . dashboard-refresh-buffer))

(use-package elfeed
  :custom
  (elfeed-feeds '("https://port19.xyz/rss.xml"
                  "https://mitchmarq42.xyz/index.xml"
                  "https://lukesmith.xyz/index.xml"
                  "https://feeds.transistor.fm/thoughts-on-functional-programming-podcast-by-eric-normand"
                  "https://protesilaos.com/commentary.xml"
                  "https://protesilaos.com/codelog.xml"
                  "https://protesilaos.com/news.xml"
                  "https://planet.archlinux.org/rss20.xml"
                  "https://distrowatch.com/news/dwd.xml"
                  "https://clojure.org/feed.xml"
                  "https://github.blog/changelog/feed")))

(use-package doom-themes        :config (load-theme 'doom-nord-aurora t))
(use-package doom-modeline      :config (doom-modeline-mode)) ;nerd-icons-install-fonts
(use-package helpful            :custom (helpful-max-buffers 3))
(use-package beacon             :config (beacon-mode 1))

(use-package vertico            :config (vertico-mode)
                                :custom (vertico-resize t))
(use-package orderless          :custom (completion-styles '(orderless basic)))
(use-package marginalia         :config (marginalia-mode))
(use-package which-key          :config (which-key-mode)
                                :custom (which-key-sort-order 'which-key-key-order-alpha))
(use-package corfu              :custom (corfu-auto t)
                                :config (global-corfu-mode))
(use-package consult)
(use-package consult-projectile)
(use-package projectile         :config (projectile-mode +1))

(use-package eshell-toggle      :custom (eshell-toggle-size-fraction 4))
(use-package vterm              :custom (vterm-always-compile-module t))
(use-package pdf-tools          :magic  ("%PDF" . pdf-view-mode)
                                :config (pdf-tools-install :no-query))
(use-package org-superstar      :hook   (org-mode . org-superstar-mode))

(use-package evil               :init   (setq evil-want-keybinding nil)
                                :config (evil-mode 1)
                                :custom (evil-undo-system 'undo-redo))
(use-package evil-goggles       :config (evil-goggles-mode))
(use-package evil-vimish-fold   :config (global-evil-vimish-fold-mode))
(use-package evil-collection    :config (evil-collection-init))

(use-package clojure-mode       :mode   "\\.edn\\'" "\\.clj?[scx]\\'")
(use-package cider              :custom (cider-repl-pop-to-buffer-on-connect . nil))
(use-package clj-refactor       :custom (cljr-project-clean-prompt nil))
(use-package rainbow-delimiters :hook   (prog-mode . rainbow-delimiters-mode))
(use-package paredit            :hook   (clojure-mode . paredit-mode))
(use-package eglot              :hook   (clojure-mode . eglot-ensure))

(use-package magit              :custom (magit-slow-confirm nil))
(use-package hl-todo            :config (global-hl-todo-mode))
(use-package git-gutter         :config (global-git-gutter-mode))
(use-package markdown-mode      :mode   "\\.md\\'")
(use-package lua-mode           :mode   "\\.lua\\'")
(use-package ansible            :mode   "\\.ya?ml\\'")

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer my-leader-keys
    :keymaps '(normal visual insert emacs)
    :prefix "SPC"
    :global-prefix "C-M-SPC")

  (defvar my-help-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "f") #'helpful-callable)
      (define-key map (kbd "i") #'consult-info)
      (define-key map (kbd "k") #'helpful-key)
      (define-key map (kbd "m") #'describe-mode)
      (define-key map (kbd "M") #'consult-man)
      (define-key map (kbd "p") #'helpful-at-point)
      (define-key map (kbd "s") #'helpful-symbol)
      (define-key map (kbd "v") #'helpful-variable)
      map))

  (defvar my-buffer-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "b") #'consult-buffer)
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
      (define-key map (kbd "g") #'consult-flymake)
      (define-key map (kbd "h") #'clojure-cycle-privacy)
      (define-key map (kbd "i") #'cider-inspect-last-result)
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
      (define-key map (kbd "a") #'org-attach)
      (define-key map (kbd "b") #'org-insert-structure-template)
      (define-key map (kbd "c") #'org-cite-insert)
      (define-key map (kbd "d") #'org-deadline)
      (define-key map (kbd "e") #'org-export-dispatch)
      (define-key map (kbd "j") #'consult-org-heading)
      (define-key map (kbd "l") #'org-insert-link)
      (define-key map (kbd "n") #'org-narrow-to-subtree)
      (define-key map (kbd "N") #'widen)
      (define-key map (kbd "t") #'org-insert-time-stamp)
      (define-key map (kbd "e") #'org-babel-execute-src-block)
      (define-key map (kbd "p") #'org-beamer-export-to-pdf)
      map))

  (my-leader-keys
    "a" '(org-agenda :which-key "org agenda")
    "b" `(,my-buffer-map :which-key "Buffer")
    "c" '(magit-clone :which-key "magit clone")
    "d" '(dired-jump :which-key "dired jump")
    "e" '(eshell-toggle :which-key "eshell")
    "f" '(find-file :which-key "open file")
    "F" '(consult-find :which-key "consult find")
    "g" '(magit :which-key "magit")
    "G" '(consult-ripgrep :which-key "consult grep")
    "h" `(,my-help-map :which-key "Help")
    "H" '(consult-history :which-key "history completion")
    "i" '(insert-char :which-key "insert unicode")
    "j" '(consult-imenu :which-key "jump via imenu")
    "k" '(comment-region :which-key "comment region")
    "l" '(org-store-link :which-key "org store link")
    "m" '(hl-todo-next :which-key "next Todo")
    "n" '(elfeed :which-key "news (elfeed)")
    "o" `(,my-org-map :which-key "Org Mode")
    "p" '(consult-projectile-find-file :which-key "hop project file")
    "P" '(consult-projectile-switch-project :which-key "hop project")
    "q" '(save-buffers-kill-emacs :which-key "quit emacs")
    "r" '(consult-recent-file :which-key "open recent")
    "R" '(re-builder :which-key "regex builder") ;consider pcre replacement
    "s" '(consult-line :which-key "seek")
    "t" '(vterm-other-window :which-key "vterm")
    "u" '(consult-theme :which-key "change theme")
    "v" '(eval-last-sexp :which-key "(emacs) eval")
    "w" `(,my-window-map :which-key "Windows")
    "x" '(org-capture :which-key "org capture")
    ;y elpaca try
    "z" '(zone :which-key "zone")
    "SPC" `(,my-clojure-map :which-key "Clojure")
    "<return>" '(consult-bookmark :which-key "jump to bookmark")
    "S-<return>" '(bookmark-set :which-key "set a bookmark")))
