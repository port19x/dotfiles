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
(use-package emacs               :config  (menu-bar-mode -1)
                                          (tool-bar-mode -1)
                                          (scroll-bar-mode -1)
                                          (horizontal-scroll-bar-mode -1)
                                          (savehist-mode 1)
                                 :hook    (prog-mode display-line-numbers-mode)
                                 :custom  (inhibit-startup-message t)
                                          (custom-file (concat user-emacs-directory "/custom.el"))
                                          (display-line-numbers-type `relative)
                                          (native-comp-deferred-compilation nil)
                                          (dired-kill-when-opening-new-dired-buffer t)
                                          (indent-tabs-mode nil)
                                          (ubiquify-buffer-name-style 'forward)
                                          (visible-bell t)
                                          (user-full-name "port19")
                                          (user-mail-address "port19@port19.xyz")
                                          (org-directory "~/doc/org"))

;;; >PACKAGES< ;;;
;;; dashboard ;;;
(use-package dashboard           :preface (defun my/dashboard-banner ()
                                            (setq dashbard-banner-logo-title
                                                  (format "Emacs ready in %s seconds with %d garbage collections."
                                                          (emacs-init-time) gcs-done)))
                                 :custom  (dashboard-startup-banner "~/dotfiles/emacs/avatar.gif")
                                          (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
                                          (dashboard-center-content t)
                                          (dashboard-items '((recents  . 5) (bookmarks . 5) (projects . 5)))
                                 :config  (dashboard-setup-startup-hook)
                                 :hook    ((after-init     . dashboard-refresh-buffer)
                                           (dashboard-mode . my/dashboard-banner)))

;;; look and feel ;;;
(set-face-attribute 'default nil :font "Iosevka" :height 140)
(use-package doom-themes         :config  (load-theme 'doom-nord-aurora t))
(use-package doom-modeline       :config  (doom-modeline-mode))

;;; completion ;;;
(use-package vertico             :custom  (vertico-resize t)
                                 :config  (vertico-mode))
(use-package marginalia          :config  (marginalia-mode))
(use-package which-key           :config  (which-key-mode)
                                 :custom  (which-key-max-display-columns 3))
(use-package projectile          :config  (projectile-mode +1))
(use-package corfu               :custom  (corfu-auto t)
                                 :config  (global-corfu-mode))
(use-package helpful)

;;; evil keys ;;;
(use-package evil                :init    (setq evil-want-keybinding nil)
                                 :config  (evil-mode 1)
                                 :custom  (evil-undo-system 'undo-redo))
(use-package evil-goggles        :config  (evil-goggles-mode))
(use-package evil-vimish-fold    :config  (global-evil-vimish-fold-mode))
(use-package evil-collection     :config  (evil-collection-init))

;;; clojure ;;;
;;; TODO bind several cider keys to the cider keymap
(use-package clojure-mode        :mode    "\\.edn\\'" "\\.clj?[scx]\\'")
(use-package cider               :after   (clojure-mode))
(use-package rainbow-delimiters  :hook    (prog-mode . rainbow-delimiters-mode))
(use-package smartparens         :hook    (prog-mode . smartparens-mode))
(use-package format-all          :hook    (clojure-mode . format-all-mode))
(use-package paredit             :hook    (clojure-mode . paredit-mode)
                                 :bind    (:map clojure-mode-map ("<" . paredit-forward-slurp-sexp)))
(use-package lsp-mode            :hook    (clojure-mode . lsp))
(use-package lsp-ui              :after   (lsp-mode)
                                 :custom  (lsp-ui-doc-show-with-cursor t)
                                          (lsp-ui-doc-delay 2.0))

;;; other programming related modes ;;;
(use-package magit)
(use-package hl-todo             :config  (global-hl-todo-mode))
(use-package git-gutter          :config  (global-git-gutter-mode))
(use-package markdown-mode       :mode    "\\.md\\'")
(use-package lua-mode            :mode    "\\.lua\\'")
(use-package ansible             :mode    "\\.ya?ml\\'")

;;; media related stuff ;;;
(use-package org-superstar       :hook    (org-mode . org-superstar-mode))
(use-package pdf-tools           :magic   ("%PDF" . pdf-view-mode)
                                 :config  (pdf-tools-install :no-query))

;;; >KEYBINDINGS< ;;;
(use-package general
  :config
  (general-evil-setup)

  ;; Here we define the leader key which works in Evil normal and
  ;; visual states.  We then define our own keymaps to group commands
  ;; under a common key.
  (general-create-definer basic-emacs-leader-keys
    :keymaps '(normal visual insert emacs)
    :prefix "SPC"
    :global-prefix "C-M-SPC")

  (defvar basic-emacs-file-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "f") #'find-file)
      (define-key map (kbd "F") #'find-file-other-window)
      (define-key map (kbd "r") #'recentf-open)
      (define-key map (kbd "s") #'save-buffer)
      (define-key map (kbd "w") #'write-file)
      (define-key map (kbd "d") #'dired)
      (define-key map (kbd "D") #'dired-other-window)
      map)
    "Custom keymap with file-related commands.
Add this to `basic-emacs-leader-keys'.")

  (defvar basic-emacs-help-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "v") #'helpful-variable)
      (define-key map (kbd "f") #'helpful-callable)
      (define-key map (kbd "k") #'helpful-key)
      (define-key map (kbd "m") #'helpful-mode)
      (define-key map (kbd "s") #'helpful-symbol)
      (define-key map (kbd "K") #'describe-keymap)
      (define-key map (kbd "p") #'helpful-at-point)
      (define-key map (kbd "M") #'man)
      ;; TODO info interface similar to man
      map)
    "Custom keymap with file-related commands.
Add this to `basic-emacs-leader-keys'.")

  (defvar basic-emacs-buffer-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "b") #'switch-to-buffer)
      (define-key map (kbd "B") #'switch-to-buffer-other-window)
      (define-key map (kbd "c") #'clone-indirect-buffer)
      (define-key map (kbd "C") #'clone-indirect-buffer-other-window)
      (define-key map (kbd "k") #'kill-current-buffer)
      (define-key map (kbd "r") #'revert-buffer)
      (define-key map (kbd "R") #'rename-buffer)
      (define-key map (kbd "s") #'save-buffer)
      (define-key map (kbd "n") #'next-buffer)
      (define-key map (kbd "p") #'previous-buffer)
      (define-key map (kbd "m") #'buffer-menu)
      (define-key map (kbd "q") #'bury-buffer)
      map)
    "Custom keymap with buffer-related commands.
Add this to `basic-emacs-leader-keys'.")

  (defvar basic-emacs-window-map
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
      map)
    "Custom keymap with various window commands.
Add this to `basic-emacs-leader-keys'.")

  ;; FIXME Register the window resize commands in the `repeat-mode' map.
  (dolist (cmd '( enlarge-window enlarge-window-horizontally
                  shrink-window shrink-window-horizontally
                  evil-window-left evil-window-down
                  evil-window-up evil-window-right))
    (put cmd 'repeat-map 'basic-emacs-window-map))

  (basic-emacs-leader-keys
    "<return>" '(bookmark-jump :which-key "Jump to bookmark")
    "S-<return>" '(bookmark-set :which-key "Set a bookmark")
    "b" `(,basic-emacs-buffer-map :which-key "Buffer")
    "s" '(isearch-forward-regexp :which-key "Seek")
    "f" `(,basic-emacs-file-map :which-key "File")
    "h" `(,basic-emacs-help-map :which-key "Help")
    "i" '(insert-char :which-key "Insert Unicode")
    "v" '(magit :which-key "Run Magit")
    "c" '(org-capture :which-key "Org Capture")
    "p" `(,project-prefix-map :which-key "Projects")
    "q" '(save-buffers-kill-emacs :which-key "Quit Emacs")
    "w" `(,basic-emacs-window-map :which-key "Windows")))
