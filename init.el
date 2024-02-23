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

(use-package emacs
  :config
  (display-time-mode 1)
  (global-auto-revert-mode 1)
  (save-place-mode 1)
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t) (shell . t)))
  (require 'oc-biblatex)
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'default nil :font "Iosevka" :height 140)
  :custom
  (custom-file (concat user-emacs-directory "/custom.el"))
  (dired-kill-when-opening-new-dired-buffer t)
  (eshell-history-size 100000)
  (inhibit-startup-message t)
  (make-backup-files nil)
  (org-startup-indented t)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate nil)
  (org-latex-pdf-process '("%latex -interaction nonstopmode -output-directory %o %f"
                           "%bib %b"
                           "%latex -interaction nonstopmode -output-directory %o %f"
                           "%latex -interaction nonstopmode -output-directory %o %f"))
  (org-agenda-files '("~/doc/master.org"))
  (org-agenda-restore-windows-after-quit t)
  (org-capture-templates '(("a" "Appointment" entry (file+headline "~/doc/master.org" "📅 Agenda") "** %t ")
                           ("t" "Todo" entry (file+headline "~/doc/master.org" "📅 Agenda") "** TODO ")))
  :hook
  (after-init . (lambda () (setq gc-cons-threshold (* 8 1024 1024))))
  (dired-mode . dired-hide-details-mode)
  (prog-mode . electric-pair-mode)
  (python-mode . flymake-mode)
  (sh-mode . flymake-mode)
  (org-mode . hl-todo-mode)
  (org-mode . visual-line-mode))

(use-package better-defaults)
(use-package define-word)
(use-package helpful)
;completion
(use-package embark-consult)
(use-package orderless            :custom (completion-styles '(orderless basic)) (orderless-matching-styles '(orderless-flex)))
(use-package marginalia           :config (marginalia-mode))
(use-package vertico              :config (vertico-mode)      :custom (vertico-resize t))
(use-package corfu                :config (global-corfu-mode) :custom (corfu-auto t))
(use-package which-key            :config (which-key-mode)    :custom (which-key-sort-order 'which-key-key-order-alpha))
;term
(use-package vterm                :custom (vterm-always-compile-module t))
(use-package eshell-toggle        :bind   ((:map eshell-mode-map ("<right>" . capf-autosuggest-accept))))
(use-package capf-autosuggest     :hook   eshell-mode)
;git -- (let ((projectile-project-search-path '("~/git/"))) (projectile-discover-projects-in-search-path))
(use-package consult-git-log-grep :custom (consult-git-log-grep-open-function 'magit-show-commit))
(use-package git-gutter           :config (global-git-gutter-mode))
(use-package projectile           :config (projectile-mode +1)
                                  :custom (projectile-completion-system 'default))
(use-package magit                :hook   (projectile-after-switch-project . vc-pull))
(use-package evil-collection      :config (evil-collection-init) :after evil)
;visual -- (nerd-icons-install-font)
(use-package evil-goggles         :config (evil-goggles-mode) :after evil)
(use-package doom-modeline        :config (doom-modeline-mode))
(use-package nerd-icons-dired     :hook   dired-mode)
(use-package org-modern           :hook   org-mode)
(use-package dired-filter         :hook   (dired-mode . dired-filter-by-dot-files))
(use-package hl-todo              :config (global-hl-todo-mode)
                                  :custom (hl-todo--regexp "\\(\\<\\(TODO\\|CITE\\|IMAGE\\|LINK\\)\\>\\)")
                                          (hl-todo-keyword-faces '(("TODO" . "#cc9393") ("CITE" . "#dc8cc3") ("IMAGE" . "#dc8cc3") ("LINK" . "#dc8cc3"))))
(use-package dashboard            :custom (dashboard-center-content t)
                                          (dashboard-startup-banner "~/pic/emacschan.png")
                                  :hook   (after-init . dashboard-refresh-buffer))
;specifics -- (pdf-loader-install t)
(use-package pdf-tools            :mode   "\\.pdf\\'")
(use-package elfeed               :custom (elfeed-feeds '("https://sachachua.com/blog/category/emacs-news/feed/" "https://blog.fefe.de/rss.xml?html" "https://clojure.org/feed.xml")))
(use-package markdown-mode        :mode   "\\.md\\'")
(use-package paredit              :hook   prog-mode)
(use-package rainbow-delimiters   :hook   prog-mode)

;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(use-package reformatter          :config (reformatter-define shfmt :program "shfmt" :args (list "--filename" (or (buffer-file-name) input-file) "-i" "4" "-ci"))
                                  :custom (treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")))
                                          (major-mode-remap-alist '((sh-mode . bash-ts-mode)))
                                  :hook   (bash-ts-mode . shfmt-on-save-mode))

(use-package exwm                 :init   (exwm-enable)
                                  :config (add-to-list 'exwm-input-prefix-keys ?\M- )
                                  :custom (exwm-workspace-number 3)
                                  :hook (exwm-update-class . (lambda () (exwm-workspace-rename-buffer exwm-class-name))))

; TODO try meow
(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1)
          (define-key evil-motion-state-map "," nil)
  :custom (evil-undo-system 'undo-redo))

(use-package general
  :config
  (defun toggle-org-pdf-export-on-save () (interactive)
    (if (memq 'org-latex-export-to-pdf after-save-hook)
        (progn
          (remove-hook 'after-save-hook 'org-latex-export-to-pdf t)
          (message "Disabled org latex export on save for current buffer..."))
      (add-hook 'after-save-hook 'org-latex-export-to-pdf nil t)
      (message "Enabled org latex export on save for current buffer...")))
  (defun my-info-read-manual () (interactive)
         (info (completing-read "Info entry: " (info--manual-names nil))))
  (defun launch (command)
      (interactive (list (read-shell-command "$ ")))
      (start-process-shell-command command nil command))
  (defun slock () (interactive) (launch "slock"))
  (defun qutebrowser () (interactive) (launch "qutebrowser"))
  (defun flameshot () (interactive) (launch "flameshot gui"))
  (defun org-preferred-agenda () (interactive) (org-agenda "" "n"))

 (defvar my-help-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "d") #'define-word)
      (define-key map (kbd "D") #'define-word-at-point)
      (define-key map (kbd "h") #'helpful-symbol)
      (define-key map (kbd "i") #'my-info-read-manual)
      (define-key map (kbd "k") #'helpful-key)
      (define-key map (kbd "m") #'describe-mode)
      (define-key map (kbd "M") #'man)
      (define-key map (kbd "p") #'helpful-at-point)
      map))
  (defvar my-buffer-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "b") #'consult-buffer)
      (define-key map (kbd "i") #'ibuffer)
      (define-key map (kbd "k") #'kill-current-buffer)
      (define-key map (kbd "n") #'next-buffer)
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
      (define-key map (kbd "c") #'org-cite-insert)
      (define-key map (kbd "e") #'org-babel-execute-src-block)
      (define-key map (kbd "h") #'org-html-export-to-html)
      (define-key map (kbd "i") #'org-indent-mode)
      (define-key map (kbd "j") #'consult-org-heading)
      (define-key map (kbd "k") #'org-clock-in)
      (define-key map (kbd "K") #'org-clock-out)
      (define-key map (kbd "n") #'org-narrow-to-subtree)
      (define-key map (kbd "N") #'widen)
      (define-key map (kbd "p") #'org-latex-export-to-pdf)
      (define-key map (kbd "P") #'org-beamer-export-to-pdf)
      (define-key map (kbd "s") #'org-cut-subtree)
      (define-key map (kbd "t") #'org-time-stamp)
      (define-key map (kbd "v") #'visual-line-mode)
      (define-key map (kbd "x") #'org-export-dispatch)
      (define-key map (kbd "X") #'toggle-org-pdf-export-on-save)
      (define-key map (kbd "SPC") #'embark-act)
      map))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "a" '(org-preferred-agenda :which-key "agenda")
   "b" `(,my-buffer-map :which-key "Buffer")
   "B" '(magit-blame-addition :which-key "git blame")
   "c" '(org-capture :which-key "capture")
   "C" '(magit-clone :which-key "magit clone")
   "d" '(dired-jump :which-key "dired jump")
   "e" '(eshell-toggle :which-key "eshell")
   "E" '(vterm :which-key "vterm")
   "f" '(find-file :which-key "open file")
   "F" '(consult-fd :which-key "consult find")
   "g" '(magit :which-key "magit")
   "G" '(consult-ripgrep :which-key "consult grep")
   "h" `(,my-help-map :which-key "Help")
   "i" '(insert-char :which-key "insert unicode")
   "j" '(consult-imenu :which-key "jump via imenu")
   "k" '(comment-region :which-key "comment region")
   "K" '(indent-region :which-key "indent region")
   "l" '(slock :which-key "lock screen")
   "L" '(consult-git-log-grep :which-key "grep git log")
   "m" '(hl-todo-next :which-key "next Todo")
   "n" '(elfeed :which-key "news (elfeed)")
   "p" '(projectile-find-file :which-key "hop project file")
   "P" '(projectile-switch-project :which-key "hop project")
   "q" '(qutebrowser :which-key "launch browser")
   "Q" '(save-buffers-kill-emacs :which-key "quit emacs")
   "r" '(consult-recent-file :which-key "open recent")
   "R" '(launch :which-key "launcher")
   "s" '(flameshot :which-key "screenshot")
   "u" '(consult-theme :which-key "change theme")
   "v" '(eval-last-sexp :which-key "(emacs) eval")
   "w" `(,my-window-map :which-key "Windows")
   "x" '(consult-flymake :which-key "run linters (flymake)")
   "z" '(dashboard-refresh-buffer :which-key "Dashboard")
   "SPC" `(,my-org-map :which-key "Org Mode")
   "<" '(paredit-forward-slurp-sexp :which-key "Paren Slurp")
   "<return>" '(consult-bookmark :which-key "jump to bookmark")
   "S-<return>" '(bookmark-set :which-key "set a bookmark")))
