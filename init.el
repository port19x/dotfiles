(eval-when-compile (require 'use-package))
(setq gc-cons-threshold most-positive-fixnum
      package-native-compile t
      use-package-always-ensure t
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(use-package no-littering)

(use-package emacs
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (savehist-mode 1)
  (display-time-mode 1)
  (global-auto-revert-mode 1)
  (save-place-mode 1)
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'default nil :font "Iosevka" :height 140)
  :custom
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
  (python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
  (treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                                   (go "https://github.com/tree-sitter/tree-sitter-go")
                                   (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
                                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                                   (php "https://github.com/tree-sitter/tree-sitter-php")
                                   (python "https://github.com/tree-sitter/tree-sitter-python")
                                   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (major-mode-remap-alist '((sh-mode . bash-ts-mode)
                            (python-mode . python-ts-mode)))
  (org-startup-indented t)
  :hook
  (after-init . (lambda () (setq gc-cons-threshold (* 8 1024 1024))))
  (dired-mode . dired-hide-details-mode)
  (prog-mode . electric-pair-mode)
  (python-ts-mode . flymake-mode)
  (bash-ts-mode . flymake-mode)
  (web-mode . flymake-mode)
  (org-mode . hl-todo-mode)
  (org-mode . visual-line-mode))

(use-package define-word)
(use-package tldr)
(use-package embark-consult)
(use-package wgrep)
(use-package paredit)
(use-package eat)
(use-package orderless            :custom (completion-styles '(orderless basic)))
(use-package marginalia           :config (marginalia-mode))
(use-package vertico              :config (vertico-mode))
(use-package corfu                :config (global-corfu-mode) :custom (corfu-auto t))
(use-package which-key            :config (which-key-mode)    :custom (which-key-sort-order 'which-key-key-order-alpha)) ;emacs30
(use-package capf-autosuggest     :hook   shell-mode)
(use-package git-gutter           :config (global-git-gutter-mode))
(use-package git-link             :config (progn (add-to-list 'git-link-remote-alist '("git.*" git-link-gitea))
                                                 (add-to-list 'git-link-commit-remote-alist '("git.*" git-link-commit-gitea))))
(use-package magit                :custom (magit-slow-confirm nil))
(use-package org-modern           :custom (org-modern-star 'replace) :hook   org-mode)
(use-package dashboard            :custom (dashboard-center-content t)
                                          (dashboard-startup-banner "~/pic/dashboard.jpg")
                                  :hook   (after-init . dashboard-refresh-buffer))
(use-package shell-pop            :bind   ((:map shell-mode-map ("<right>" . capf-autosuggest-accept))))
(use-package pdf-tools            :config (pdf-tools-install t))
(use-package reformatter
  :config
  (reformatter-define shfmt :program "shfmt" :args (list "--filename" (or (buffer-file-name) input-file) "-i" "4" "-ci"))
  (reformatter-define ruff :program "ruff" :args (list "format" "--stdin-filename" (or (buffer-file-name) input-file) "--line-length" "320")))

(use-package markdown-ts-mode      :mode   "\\.md\\'")
(use-package web-mode              :mode   "\\.php\\'") ;emacs30 php-ts-mode
(use-package eros                  :hook   (emacs-lisp-mode))
(use-package rainbow-delimiters    :hook   (prog-mode))
(use-package sly-overlay :custom (inferior-lisp-program "ros -Q run")
  :hook (sly-mode . (lambda () (unless (sly-connected-p) (save-excursion (sly))))))

(use-package exwm
  :if (eq system-type 'gnu/linux)
  :init
  (require 'exwm-randr)
  (exwm-randr-mode 1)
  (start-process-shell-command "xrandr" nil "xrandr --output DP-1-1 --left-of eDP-1 --rotate right --output DP-1-2 --left-of DP-1-1 --rotate left")
  (exwm-enable)
  :config
  (add-to-list 'exwm-input-prefix-keys ?\M- )
  :custom
  (exwm-workspace-number 3)
  (exwm-randr-workspace-monitor-plist '(0 "eDP" 1 "DP-1-1" 2 "DP-1-2"))
  :hook
  (exwm-update-class . (lambda () (exwm-workspace-rename-buffer exwm-class-name))))

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1)
          (define-key evil-motion-state-map "," nil)
  :custom (evil-undo-system 'undo-redo)
          (evil-disable-insert-state-bindings t))
(use-package evil-collection      :config (evil-collection-init))
(use-package evil-goggles         :config (evil-goggles-mode))
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

 (defvar my-help-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "d") #'define-word)
      (define-key map (kbd "D") #'define-word-at-point)
      (define-key map (kbd "h") #'describe-symbol)
      (define-key map (kbd "i") #'my-info-read-manual)
      (define-key map (kbd "m") #'man)
      (define-key map (kbd "t") #'tldr)
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

  (defvar my-lisp-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "SPC") #'sly-overlay-eval-defun)
      (define-key map (kbd "b") #'sly-eval-buffer)
      (define-key map (kbd "c") #'mrepl-clear-repl)
      (define-key map (kbd "d") #'sly-eval-defun)
      (define-key map (kbd "v") #'sly-interrupt)
      (define-key map (kbd "i") #'sly-inspect) ;TODO fix upstream profiler
      (define-key map (kbd "f") #'sly-describe-function)
      (define-key map (kbd "h") #'sly-apropos-all)
      (define-key map (kbd "q") #'sly-quit-lisp)
      (define-key map (kbd "s") #'sly-describe-symbol)
      (define-key map (kbd "w") #'sly-hyperspec-lookup)
      map))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "b" `(,my-buffer-map :which-key "Buffer")
   "B" '(magit-blame-addition :which-key "git blame")
   "c" '(comp-dwim :which-key "compile")
   "C" '(magit-clone :which-key "magit clone")
   "d" '(dired-jump :which-key "dired jump")
   "e" '(shell-pop :which-key "eshell")
   "E" '(eat-other-window :which-key "eat terminal")
   "f" '(find-file :which-key "open file")
   "F" '(consult-fd :which-key "consult find")
   "g" '(magit :which-key "magit")
   "G" '(consult-ripgrep :which-key "consult grep")
   "h" `(,my-help-map :which-key "Help")
   "i" '(insert-char :which-key "unicode insert")
   "k" '(comment-region :which-key "comment region")
   "K" '(indent-region :which-key "indent region")
   "l" '(slock :which-key "lock screen")
   "o" `(,my-org-map :which-key "Org Mode")
   "p" '(project-find-file :which-key "hop project file")
   "P" '(project-switch-project :which-key "hop project")
   "q" '(brave :which-key "launch browser")
   "Q" '(save-buffers-kill-emacs :which-key "quit emacs")
   "r" '(consult-recent-file :which-key "open recent")
   "R" '(launch :which-key "launcher")
   "s" '(flameshot :which-key "screenshot")
   "v" '(eval-last-sexp :which-key "(emacs) eval")
   "w" `(,my-window-map :which-key "Windows")
   "x" '(consult-flymake :which-key "run linters (flymake)")
   "y" '(git-link :which-key "git link")
   "z" '(dashboard-refresh-buffer :which-key "Dashboard")
   "SPC" `(,my-lisp-map :which-key "lisp map")
   "<" '(paredit-forward-slurp-sexp :which-key "Paren Slurp")
   "<return>" '(consult-bookmark :which-key "jump to bookmark")
   "S-<return>" '(bookmark-set :which-key "set a bookmark")))

;; (progn
;;   (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
;;   (pdf-loader-install t)
;;   (project-remember-projects-under "~/git" t))
;;   (setq esup-user-init-file "~/dotfiles/init.el" esup-depth 0)
