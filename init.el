(setq gc-cons-threshold most-positive-fixnum)
(setq package-native-compile t)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(eval-after-load 'gnutls '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem")) ;; axe from here (in v29)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(eval-when-compile (require 'use-package)) 
(require 'bind-key) ;; too here
(setq use-package-always-ensure t
      use-package-always-demand t)
(use-package no-littering)

(use-package emacs
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (savehist-mode 1)
  (display-time-mode 1)
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
  (after-init . (lambda () (set-face-attribute 'default nil :font "Iosevka" :height 140)))
  (after-init . (lambda () (setq gc-cons-threshold (* 8 1024 1024))))
  (after-init . elfeed-update))

(use-package org-modern
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sqlite . t)
     (shell . t)
     (python . t)
     (emacs-lisp . nil)))
  (require 'oc-biblatex)
  :custom
  (org-startup-indented t)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  (org-directory "~/.cache/org")
  (org-confirm-babel-evaluate nil)
  :hook
  (org-mode . org-modern-mode)
  (org-mode . hl-todo-mode)
  (org-mode . visual-line-mode))

(use-package tempel
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))
  :hook
  (bibtex-mode . tempel-setup-capf))

(use-package dashboard
  :custom
  (dashboard-startup-banner "~/dotfiles/orb.jpg")
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-center-content t)
  (dashboard-items '((recents  . 5) (bookmarks . 5) (projects . 5)))
  :hook
  (after-init . dashboard-refresh-buffer))

(use-package elfeed
  :custom (elfeed-feeds '("https://planet.archlinux.org/rss20.xml" "https://distrowatch.com/news/dwd.xml"
                          "https://github.blog/changelog/feed" "https://sachachua.com/blog/category/emacs-news/feed/"
                          "https://openrss.org/github.com/pystardust/ani-cli/issues" "https://openrss.org/github.com/pystardust/ani-cli/pulls"
                          "https://feeds.feedburner.com/HaveIBeenPwnedLatestBreaches" "https://blog.fefe.de/rss.xml?html")))

; UI
(use-package exwm               :init   (require 'exwm)
                                        (exwm-enable)
                                :config (add-to-list 'exwm-input-prefix-keys ?\M- )
                                :custom (exwm-workspace-number 1)
                                :hook   (exwm-update-class . (lambda () (exwm-workspace-rename-buffer exwm-class-name))))
(use-package doom-themes)
(use-package ef-themes          :config (load-theme 'ef-maris-dark t))
(use-package doom-modeline      :config (doom-modeline-mode)) ;nerd-icons-install-fonts
(use-package beacon             :config (beacon-mode 1))
(use-package helpful            :custom (helpful-max-buffers 3))
(use-package define-it)
(use-package which-key          :config (which-key-mode)
                                :custom (which-key-sort-order 'which-key-key-order-alpha))

; Completion Stack
(use-package vertico            :config (vertico-mode)
                                :custom (vertico-resize t))
(use-package orderless          :custom (completion-styles '(orderless basic))
                                        (orderless-matching-styles '(orderless-flex)))
(use-package marginalia         :config (marginalia-mode))
(use-package consult-projectile :config (projectile-mode +1)
                                :custom (projectile-project-search-path '("~/git/")))
(use-package consult-ls-git)
(use-package corfu              :custom (corfu-auto t)
                                :config (global-corfu-mode))

; Programming Modes
(use-package consult-git-log-grep)
(use-package magit              :custom (magit-slow-confirm nil))
(use-package git-gutter         :config (global-git-gutter-mode))
(use-package hl-todo            :config (global-hl-todo-mode)
  :custom (hl-todo--regexp "\\(\\<\\(TODO\\|HACK\\|CITE\\|IMAGE\\|LINK\\|FIXME\\)\\>\\)")
          (hl-todo-keyword-faces '(("HACK" . "#d0bf8f")
                                   ("FIXME" . "#cc9393")
                                   ("TODO" . "#cc9393")
                                   ("CITE" . "#dc8cc3")
                                   ("IMAGE" . "#dc8cc3")
                                   ("LINK" . "#dc8cc3"))))
(use-package markdown-mode      :mode "\\.md\\'")
(use-package lua-mode           :mode "\\.lua\\'")
(use-package ansible            :mode "\\.ya?ml\\'")
(use-package docker-compose-mode)

; Multimedia
(use-package pdf-tools          :init (pdf-tools-install 1))
(use-package keepass-mode       :mode "\\.kdbx\\'")
(use-package el-fetch)

; Terminals & Aliases
(use-package eat)
(use-package esh-autosuggest)
(use-package eshell-toggle
  :init
  (defun eshell-add-aliases () ""
         (dolist (var '(("v" "find-file $1")
                        ("ap" "ansible-playbook $1")
                        ("rm" "rm -I --preserve-root $1")
                        ("ll" "ls -la")
                        ("la" "ls -a")
                        ("yta" "yt-dlp --embed-thumbnail -f 'bestaudio/best' -f 'm4a' $1")
                        ("ytd" "yt-dlp -f 'bestvideo[height<=?1080]+bestaudio/best' -f 'mp4' $1")))
      (add-to-list 'eshell-command-aliases-list var)))
  :custom
  (eshell-history-size 100000)
  :hook
  (eshell-post-command . eshell-add-aliases)
  (eshell-mode . esh-autosuggest-mode))

; Key Bindigns
(use-package evil               :init   (setq evil-want-keybinding nil)
                                :config (evil-mode 1)
                                :custom (evil-undo-system 'undo-redo))
(use-package evil-goggles       :config (evil-goggles-mode))
(use-package evil-vimish-fold   :config (global-evil-vimish-fold-mode))
(use-package evil-collection    :config (evil-collection-init))
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer my-spc-map
    :keymaps '(normal visual insert emacs)
    :prefix "SPC"
    :global-prefix "M-SPC")

  (defun my-info-read-manual ()
    (interactive)
    (info
     (completing-read "Read Info manual: "
                      (info--manual-names nil)
                      nil
                      :require-match)))

  (defun toggle-org-pdf-export-on-save ()
    (interactive)
    (if (memq 'org-latex-export-to-pdf after-save-hook)
        (progn
          (remove-hook 'after-save-hook 'org-latex-export-to-pdf t)
          (message "Disabled org latex export on save for current buffer..."))
      (add-hook 'after-save-hook 'org-latex-export-to-pdf nil t)
      (message "Enabled org latex export on save for current buffer...")))

  (defun launch (command)
      (interactive (list (read-shell-command "$ ")))
      (start-process-shell-command command nil command))
  (defun slock () (interactive) (launch "slock"))
  (defun brave () (interactive) (launch "brave"))
  (defun flameshot () (interactive) (launch "flameshot gui"))

  (defvar my-help-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "d") #'define-it)
      (define-key map (kbd "D") #'define-it-at-point)
      (define-key map (kbd "f") #'helpful-callable)
      (define-key map (kbd "i") #'my-info-read-manual)
      (define-key map (kbd "k") #'helpful-key)
      (define-key map (kbd "m") #'describe-mode)
      (define-key map (kbd "M") #'man)
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
      (define-key map (kbd "n") #'next-buffer)
      (define-key map (kbd "r") #'revert-buffer)
      (define-key map (kbd "s") #'scratch-buffer)
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

  (defvar my-org-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "SPC") #'tempel-insert)
      (define-key map (kbd "b") #'org-insert-structure-template)
      (define-key map (kbd "c") #'org-cite-insert)
      (define-key map (kbd "d") #'org-deadline)
      (define-key map (kbd "e") #'org-babel-execute-src-block)
      (define-key map (kbd "f") #'org-footnote-action)
      (define-key map (kbd "h") #'org-html-export-to-html)
      (define-key map (kbd "i") #'org-indent-mode)
      (define-key map (kbd "j") #'consult-org-heading)
      (define-key map (kbd "k") #'org-clock-in)
      (define-key map (kbd "K") #'org-clock-out)
      (define-key map (kbd "l") #'org-insert-link)
      (define-key map (kbd "n") #'org-narrow-to-subtree)
      (define-key map (kbd "m") #'org-modern-mode)
      (define-key map (kbd "N") #'widen)
      (define-key map (kbd "p") #'org-latex-export-to-pdf)
      (define-key map (kbd "P") #'org-beamer-export-to-pdf)
      (define-key map (kbd "s") #'org-cut-subtree)
      (define-key map (kbd "t") #'org-insert-time-stamp)
      (define-key map (kbd "v") #'visual-line-mode)
      (define-key map (kbd "x") #'org-export-dispatch)
      (define-key map (kbd "X") #'toggle-org-pdf-export-on-save)
      map))

  (my-spc-map
    "b" `(,my-buffer-map :which-key "Buffer")
    "c" '(magit-clone :which-key "magit clone")
    "d" '(dired-jump :which-key "dired jump")
    "e" '(eshell-toggle :which-key "eshell")
    "E" '(eat-other-window :which-key "eat")
    "f" '(find-file :which-key "open file")
    "F" '(consult-find :which-key "consult find")
    "g" '(magit :which-key "magit")
    "G" '(consult-ripgrep :which-key "consult grep")
    "h" `(,my-help-map :which-key "Help")
    "H" '(consult-history :which-key "history completion")
    "i" '(insert-char :which-key "insert unicode")
    "j" '(consult-imenu :which-key "jump via imenu")
    "k" '(comment-region :which-key "comment region")
    "K" '(indent-region :which-key "indent region")
    "l" '(slock :which-key "lock screen")
    "L" '(consult-git-log-grep :which-key "grep git log")
    "m" '(hl-todo-next :which-key "next Todo")
    "n" '(elfeed :which-key "news (elfeed)")
    "p" '(consult-ls-git :which-key "hop project file")
    "P" '(consult-projectile-switch-project :which-key "hop project")
    "q" '(brave :which-key "launch browser")
    "Q" '(save-buffers-kill-emacs :which-key "quit emacs")
    "r" '(consult-recent-file :which-key "open recent")
    "R" '(launch :which-key "launcher")
    "s" '(flameshot :which-key "screenshot")
    "u" '(consult-theme :which-key "change theme")
    "v" '(eval-last-sexp :which-key "(emacs) eval")
    "w" `(,my-window-map :which-key "Windows")
    "y" '(el-fetch :which-key "Fetch")
    "z" '(dashboard-refresh-buffer :which-key "Dashboard")
    "SPC" `(,my-org-map :which-key "Org Mode")
    "<return>" '(consult-bookmark :which-key "jump to bookmark")
    "S-<return>" '(bookmark-set :which-key "set a bookmark")))
