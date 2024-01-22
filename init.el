(eval-when-compile (require 'use-package))
(setq gc-cons-threshold most-positive-fixnum
      package-native-compile t
      use-package-always-ensure t
      use-package-always-demand t
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(use-package no-littering)


(use-package better-defaults)
(use-package define-word)
(use-package helpful)
;completion
(use-package orderless            :custom (completion-styles '(orderless basic)) (orderless-matching-styles '(orderless-flex)))
(use-package marginalia           :config (marginalia-mode))
(use-package vertico              :config (vertico-mode)      :custom (vertico-resize t))
(use-package corfu                :config (global-corfu-mode) :custom (corfu-auto t))
;term
(use-package vterm                :custom (vterm-always-compile-module t))
(use-package eshell-toggle        :bind   ((:map eshell-mode-map ("<right>" . capf-autosuggest-accept))))
(use-package capf-autosuggest     :hook   eshell-mode)
;git
(use-package consult-git-log-grep :custom (consult-git-log-grep-open-function 'magit-show-commit))
(use-package git-gutter           :config (global-git-gutter-mode))
(use-package projectile           :config (projectile-mode +1)
                                  :custom (projectile-completion-system 'default)) ;(let ((projectile-project-search-path '("~/git/"))) (projectile-discover-projects-in-search-path))
(use-package magit                :hook   (projectile-after-switch-project . vc-pull))
;visual
(use-package hl-todo              :config (global-hl-todo-mode))
(use-package evil-goggles         :config (evil-goggles-mode))
(use-package ef-themes            :config (load-theme 'ef-maris-dark t))
(use-package doom-modeline        :config (doom-modeline-mode)) ;nerd-icons-install-fonts
(use-package nerd-icons-dired     :hook   dired-mode)
;multimedia
(use-package elfeed               :custom (elfeed-feeds '("https://sachachua.com/blog/category/emacs-news/feed/" "https://blog.fefe.de/rss.xml?html")))
(use-package pdf-tools            :config (pdf-loader-install t))
;specifics
(use-package markdown-mode        :mode   "\\.md\\'")
(use-package flymake-ruff         :hook   (python-mode . flymake-ruff-load)
                                  :custom (flymake-ruff-program-args '("--output-format" "text" "--exit-zero" "--quiet" "--select" "ALL")))


(use-package reformatter
  :config
  (reformatter-define shfmt :program "shfmt" :args (append (list "--filename" (or (buffer-file-name) input-file)) '("-i" "4" "-ci")))
  (reformatter-define ruff :program "ruff" :args (list "format" "--stdin-filename" (or (buffer-file-name) input-file)))
  :hook
  (python-mode . ruff-on-save-mode)
  (sh-mode . shfmt-on-save-mode))

(use-package exwm
  :init
  (require 'exwm-randr)
  (exwm-randr-enable)
  ;(start-process-shell-command "xrandr" nil "xrandr --output DisplayPort-0 --mode 3840x2160 --left-of eDP")
  (start-process-shell-command "xrandr" nil "xrandr --output DP-1-1 --right-of eDP --output DP-1-2 --right-of DP-1-1")
  (start-process-shell-command "xrandr" nil "xrandr --output DP-1-1 --rotate left --output DP-1-2 --rotate right")
  (exwm-enable)
  :config
  (add-to-list 'exwm-input-prefix-keys ?\M- )
  :custom
  (exwm-workspace-number 3)
  ;(exwm-randr-workspace-monitor-plist '(0 "DisplayPort-0"))
  (exwm-randr-workspace-monitor-plist '(0 "eDP" 1 "DP-1-1" 2 "DP-1-2"))
  :hook
  (exwm-update-class . (lambda () (exwm-workspace-rename-buffer exwm-class-name))))

(use-package emacs
  :config
  (display-time-mode 1)
  (global-auto-revert-mode 1)
  (save-place-mode 1)
  :custom
  (custom-file (concat user-emacs-directory "/custom.el"))
  (dired-kill-when-opening-new-dired-buffer t)
  (eshell-history-size 100000)
  (inferior-lisp-program "/usr/bin/sbcl")
  (inhibit-startup-message t)
  (make-backup-files nil)
  :hook
  (after-init . (lambda () (set-face-attribute 'default nil :font "Iosevka" :height 140)))
  (after-init . (lambda () (setq gc-cons-threshold (* 8 1024 1024))))
  (dired-mode . dired-hide-details-mode)
  (prog-mode . electric-pair-mode)
  (python-mode . flymake-mode)
  (sh-mode . flymake-mode))

(use-package org-modern
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t) (shell . t)))
  (require 'oc-biblatex)
  :custom
  (org-startup-indented t)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate nil)
  (org-agenda-files '("~/doc/master.org"))
  (org-agenda-restore-windows-after-quit t)
  (org-capture-templates '(("a" "Appointment" entry (file+headline "~/doc/master.org" "📅 Agenda") "** %t ")
			   ("t" "Todo" entry (file+headline "~/doc/master.org" "📅 Agenda") "** TODO ")))
  :hook org-mode)

(use-package dashboard
  :custom
  (dashboard-startup-banner "~/pic/dashboard.jpg")
  (dashboard-center-content t)
  (dashboard-items '((recents  . 5) (bookmarks . 5) (projects . 5)))
  :hook (after-init . dashboard-refresh-buffer))

(use-package dired-filter         :init   (defun toggle-hide-dots () (interactive)
                                               (if (= (length dired-filter-stack) 0) (dired-filter-by-dot-files) (dired-filter-pop-all)))
                                  :hook   (dired-mode . dired-filter-by-dot-files)
                                  :bind   (:map dired-mode-map ("," . toggle-hide-dots)))

(use-package tempo
  :config
  (tempo-define-template "today"  '((format-time-string "%Y-%m-%d")) "today")
  (tempo-define-template "online" '("@online{" p "," n "  author = \"" p "\"," n "  title = \"" p "\"," n "  url = \"" p "\"," n "  date = \"" p "\"," n "}") "online")
  (tempo-define-template "book"   '("@book{" p "," n "  author = \"" p "\"," n "  title = \"" p "\"," n "  year = \"" p "\"," n "  publisher = \"" p "\"," n "}") "book")
  (tempo-define-template "fig"    '("#+CAPTION: " p n "[[./assets/" p "]]" n) "fig")
  (tempo-define-template "sfig"   '("#+CAPTION: " p n "#+ATTR_LATEX: :height 0.1\\textwidth" n "[[./assets/" p "]]" n) "sfig")
  (tempo-define-template "np"     '("#+LATEX:\\newpage" n) "np"))

(use-package evil-collection
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1)
          (define-key evil-motion-state-map "," nil)
          (evil-collection-init)
  :custom (evil-undo-system 'undo-redo))

(use-package major-mode-hydra
  :bind   (("M-SPC" . hydra-global/body))
  :config
  (defun toggle-org-pdf-export-on-save () (interactive)
	 (if (memq 'org-latex-export-to-pdf after-save-hook)
             (progn
               (remove-hook 'after-save-hook 'org-latex-export-to-pdf t)
               (message "Disabled org latex export on save for current buffer..."))
	   (add-hook 'after-save-hook 'org-latex-export-to-pdf nil t)
	   (message "Enabled org latex export on save for current buffer...")))
  (defun launch (command)
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

  (major-mode-hydra-define org-mode (:exit t)
    ("Template" (("SPC" tempo-complete-tag)
		 ("m" tempo-forward-mark)
		 ("M" tempo-backward-mark))
     "Editing"  (("b" org-insert-structure-template)
		 ("c" org-cite-insert)
		 ("e" org-babel-execute-src-block)
		 ("s" org-cut-subtree)
		 ("t" org-time-stamp))
     "Toggle"   (("n" org-narrow-to-subtree)
	         ("N" widen)
	         ("k" org-clock-in)
	         ("K" org-clock-out)
	         ("i" org-indent-mode)
	         ("v" visual-line-mode)
	         ("X" toggle-org-pdf-export-on-save))
     "Export"   (("h" org-html-export-to-html)
 	         ("p" org-latex-export-to-pdf)
	         ("P" org-beamer-export-to-pdf)
	         ("x" org-export-dispatch))))

  (pretty-hydra-define hydra-buffer (:exit t)
    ("Buffers"  (("b" consult-buffer)
		 ("l" ibuffer)
		 ("k" kill-current-buffer :color red)
		 ("n" next-buffer :color red)
		 ("S" scratch-buffer))))
  (pretty-hydra-define hydra-window (:exit t)
    ("Windows"  (("s" split-window-below)
		 ("v" split-window-right)
		 ("c" delete-window)
		 ("w" evil-window-next :color red))))
  (pretty-hydra-define hydra-help (:exit t)
    ("Describe" (("h" helpful-symbol)
		 ("k" helpful-key)
		 ("m" describe-mode)
		 ("p" helpful-at-point))
     "Define"   (("d" define-word)
	         ("D" define-word-at-point))
     "Manuals"  (("i" (info (completing-read "Info entry: " (info--manual-names nil))) "info")
		 ("M" man))))
  (pretty-hydra-define hydra-global (:exit t)
    ("Buffer"  (("i" insert-char "unicode")
	        ("k" comment-region "comment")
	        ("K" indent-region "indent")
	        ("t" hl-todo-next "next todo")
	        ("v" eval-last-sexp "eval sexp")
	        ("x" consult-flymake "lint"))
      "Files"  (("f" find-file "open")
       	        ("F" consult-find "find")
	        ("G" consult-ripgrep "grep")
	        ("r" consult-recent-file "recent")
	        ("m" consult-bookmark "bookmark")
	        ("M" bookmark-set "set bookmark"))
     "Git"     (("B" magit-blame-addition "blame")
	        ("C" magit-clone "clone")
	        ("g" magit)
	        ("L" consult-git-log-grep "log grep")
	        ("p" projectile-find-file "switch file")
	        ("P" projectile-switch-project "switch project"))
     "eLaunch" (("c" org-capture "capture")
		("d" dired-jump "dired")
		("e" eshell-toggle "eshell")
		("E" vterm)
		("n" elfeed)
		("z" dashboard-refresh-buffer "dashboard"))
     "xLaunch" (("l" (launch "slock") "lockscreen")
		("q" (launch "brave") "browser")
		("R" launch)
		("s" (launch "flameshot gui") "screenshot"))
     "Hydras"  (("SPC" major-mode-hydra "Major")
	        ("b" hydra-buffer/body "Buffer")
	        ("h" hydra-help/body "Help")
	        ("w" hydra-window/body "Window")
	        ("Q" save-buffers-kill-emacs "Exit")))))
