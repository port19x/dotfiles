(eval-when-compile (require 'use-package))
(setq gc-cons-threshold most-positive-fixnum
      package-native-compile t
      use-package-always-ensure t
      package-archives '(("gnu" . "https://elpa.gnu.org/devel/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu-devel/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(use-package no-littering)

(use-package emacs
  :config
  (display-battery-mode 1)
  (which-key-mode 1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (savehist-mode 1)
  (display-time-mode 1)
  (global-auto-revert-mode 1)
  (save-place-mode 1)
  (show-paren-mode 1)
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'default nil :font "Iosevka" :height 140)
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (custom-file (concat user-emacs-directory "/custom.el"))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-vc-rename-file t)
  (enable-recursive-minibuffers t)
  (inhibit-startup-message t)
  (make-backup-files nil)
  (indent-tabs-mode nil)
  (uniquify-buffer-name-style 'forward)
  (visible-bell t)
  (use-short-answers t)
  (org-startup-indented t)
  :hook
  (after-init . (lambda () (setq gc-cons-threshold (* 8 1024 1024))))
  (dired-mode . dired-hide-details-mode)
  (prog-mode . electric-pair-mode)
  (org-mode . hl-todo-mode)
  (org-mode . visual-line-mode))

;; Completion
(use-package consult-embark)
(use-package wgrep)
(use-package orderless            :custom (completion-styles '(orderless basic)))
(use-package marginalia           :config (marginalia-mode))
(use-package vertico              :config (vertico-mode))
(use-package corfu                :config (global-corfu-mode) :custom (corfu-auto t))

;; Git
(use-package git-gutter           :config (global-git-gutter-mode))
(use-package magit                :custom (magit-slow-confirm nil))

;; Shells
(use-package shell-pop            :bind   ((:map shell-mode-map ("<right>" . capf-autosuggest-accept))))
(use-package capf-autosuggest     :hook   (shell-mode))
(use-package vterm                :custom (vterm-always-compile-module t))

;; Interface Enhancements
(use-package bible-gateway        :after  dashboard
                                  :custom (dashboard-footer-messages (list (bible-gateway-get-verse))))
(use-package dashboard            :custom (dashboard-center-content t)
                                          (dashboard-items '((bookmarks . 5)))
                                          (dashboard-startup-banner "~/pic/dashboard.jpg")
                                          (dashboard-image-banner-max-height 1000)
                                  :hook   (after-init . dashboard-refresh-buffer))
(use-package evil-goggles         :config (evil-goggles-mode))
(use-package org-modern           :custom (org-modern-star 'replace) :hook   org-mode)

;; Lisp Development
(use-package paredit)
(use-package eros-inspector       :hook   (emacs-lisp-mode . eros-mode)
                                  :custom (inspector-switch-to-buffer nil))
(use-package rainbow-delimiters   :hook   (prog-mode))
(use-package clippy)
(use-package helpful)

;; Misc (no-littering see near top)
(use-package markdown-ts-mode     :mode   "\\.md\\'")
(use-package define-word)
(use-package pdf-tools            :config (pdf-tools-install t))
(use-package exwm
  :init (exwm-wm-mode)
  :config (add-to-list 'exwm-input-prefix-keys ?\M- )
  :hook (exwm-update-class . (lambda () (exwm-workspace-rename-buffer exwm-class-name))))

;; Keybindings
(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1)
          (define-key evil-motion-state-map "," nil)
  :custom (evil-undo-system 'undo-redo)
          (evil-disable-insert-state-bindings t))
(use-package evil-collection      :config (evil-collection-init))
(use-package general
  :config
  (defun my-info-read-manual () (interactive)
         (info (completing-read "Info entry: " (info--manual-names nil))))
  (defun comp-dwim () (interactive)
         (if (try-completion "*compilation*" (mapcar #'buffer-name (buffer-list)))
             (recompile)
           (if (project-current)
               (project-compile)
             (compile))))
  (defun launch (command)
      (interactive (list (read-shell-command "$ ")))
      (start-process-shell-command command nil command))
  (defun slock () (interactive) (launch "slock"))
  (defun brave () (interactive) (launch "brave"))
  (defun flameshot () (interactive) (launch "flameshot gui"))
  (defun insert-> () (interactive) (if (string= (buffer-name) "*vterm*") (vterm-insert ">") (insert ">")))
  (defun insert-< () (interactive) (if (string= (buffer-name) "*vterm*") (vterm-insert "<") (insert "<")))
  (defun insert-| () (interactive) (if (string= (buffer-name) "*vterm*") (vterm-insert "|") (insert "|")))

 (defvar my-help-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "d") #'define-word)
      (define-key map (kbd "D") #'define-word-at-point)
      (define-key map (kbd "v") #'clippy-describe-variable)
      (define-key map (kbd "f") #'clippy-describe-function)
      (define-key map (kbd "h") #'helpful-symbol)
      (define-key map (kbd "i") #'my-info-read-manual)
      (define-key map (kbd "m") #'man)
      map))
  (defvar my-buffer-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "b") #'consult-buffer)
      (define-key map (kbd "i") #'ibuffer)
      (define-key map (kbd "k") #'kill-current-buffer)
      (define-key map (kbd "s") #'scratch-buffer)
      map))
  (defvar my-window-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "s") #'split-window-below)
      (define-key map (kbd "v") #'split-window-right)
      (define-key map (kbd "c") #'delete-window)
      (define-key map (kbd "w") #'evil-window-next)
      map))

  (defvar my-org-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "b") #'org-insert-structure-template)
      (define-key map (kbd "e") #'org-babel-execute-src-block)
      (define-key map (kbd "s") #'org-cut-subtree)
      (define-key map (kbd "t") #'org-time-stamp)
      map))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "a" '(insert-| :which-key "|")
   "b" `(,my-buffer-map :which-key "Buffer")
   "B" '(magit-blame-addition :which-key "git blame")
   "c" '(comp-dwim :which-key "compile")
   "C" '(magit-clone :which-key "magit clone")
   "d" '(dired-jump :which-key "dired jump")
   "e" '(shell-pop :which-key "toggle shell")
   "E" '(vterm :which-key "vterm")
   "f" '(find-file :which-key "open file")
   "F" '(consult-find :which-key "consult find")
   "g" '(magit :which-key "magit")
   "G" '(consult-grep :which-key "consult grep")
   "h" `(,my-help-map :which-key "Help")
   "i" '(insert-char :which-key "unicode insert")
   "j" '(insert-> :which-key ">")
   "J" '(insert-< :which-key "<")
   "k" '(comment-region :which-key "comment region")
   "K" '(indent-region :which-key "indent region")
   "l" '(slock :which-key "lock screen")
   "p" '(project-find-file :which-key "hop project file")
   "P" '(project-switch-project :which-key "hop project")
   "q" '(brave :which-key "launch browser")
   "Q" '(save-buffers-kill-emacs :which-key "quit emacs")
   "r" '(consult-recent-file :which-key "open recent")
   "R" '(launch :which-key "launcher")
   "s" '(flameshot :which-key "screenshot")
   "v" '(eval-last-sexp :which-key "(emacs) eval")
   "w" `(,my-window-map :which-key "Windows")
   "z" '(dashboard-refresh-buffer :which-key "Dashboard")
   "SPC" `(,my-org-map :which-key "Org Mode")
   "<tab>" '(paredit-forward-slurp-sexp :which-key "Paren Slurp")
   "<return>" '(consult-bookmark :which-key "jump to bookmark")
   "S-<return>" '(bookmark-set :which-key "set a bookmark")))

;; (progn
;;   (project-remember-projects-under "~/git" t)
;;   (nerd-icons-install-fonts))
;;   (setq esup-user-init-file "~/dotfiles/init.el" esup-depth 0)
