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
(load-theme 'modus-vivendi t)

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
(use-package dired-filter         :hook   (dired-mode . dired-filter-by-dot-files))
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

(use-package emacs
  :config
  (display-time-mode 1)
  (global-auto-revert-mode 1)
  (save-place-mode 1)
  :custom
  (custom-file (concat user-emacs-directory "/custom.el"))
  (dired-kill-when-opening-new-dired-buffer t)
  (eshell-history-size 100000)
  (inhibit-startup-message t)
  (make-backup-files nil)
  :hook
  (after-init . (lambda () (set-face-attribute 'default nil :font "Iosevka" :height 140)))
  (after-init . (lambda () (setq gc-cons-threshold (* 8 1024 1024))))
  (dired-mode . dired-hide-details-mode)
  (prog-mode . electric-pair-mode)
  (python-mode . flymake-mode)
  (sh-mode . flymake-mode))

; TODO try meow
(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1)
          (define-key evil-motion-state-map "," nil)
  :custom (evil-undo-system 'undo-redo))

(use-package major-mode-hydra
  :demand t
  :bind   (("M-SPC" . hydra-global/body))
  :config (defun launch (command)
            (interactive (list (read-shell-command "$ ")))
            (start-process-shell-command command nil command))
  (pretty-hydra-define hydra-buffer (:exit t)
    ("Buffers"  (("1" (exwm-workspace-move-window 0) "ws 1")
                 ("2" (exwm-workspace-move-window 1) "ws 2")
                 ("3" (exwm-workspace-move-window 2) "ws 3")
                 ("b" consult-buffer)
                 ("i" ibuffer)
                 ("k" kill-current-buffer :color red)
                 ("n" next-buffer :color red)
                 ("s" scratch-buffer))))
  (pretty-hydra-define hydra-window (:exit t)
    ("Windows"  (("1" (exwm-workspace-switch 0) "ws 1")
                 ("2" (exwm-workspace-switch 1) "ws 2")
                 ("3" (exwm-workspace-switch 2) "ws 3")
                 ("s" split-window-below)
                 ("v" split-window-right)
                 ("c" delete-window)
                 ("w" evil-window-next)
                 ("b" balance-windows)
                 ("j" enlarge-window :color red)
                 ("l" enlarge-window-horizontally :color red)
                 ("k" shrink-window :color red)
                 ("h" shrink-window-horizontally :color red))))
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
                ("m" hl-todo-next "next todo")
                ("v" eval-last-sexp "eval sexp")
                ("x" consult-flymake "lint")
                ("<" paredit-forward-slurp-sexp "slurp"))
      "Files"  (("f" find-file "open")
                ("F" consult-fd "find")
                ("G" consult-ripgrep "grep")
                ("r" consult-recent-file "recent")
                ("<return>" consult-bookmark "bookmark")
                ("S-<return>" bookmark-set "set bookmark"))
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
                ("q" (launch "qutebrowser") "browser")
                ("R" launch)
                ("s" (launch "flameshot gui") "screenshot"))
     "Hydras"  (("SPC" major-mode-hydra "Major")
                ("b" hydra-buffer/body "Buffer")
                ("h" hydra-help/body "Help")
                ("w" hydra-window/body "Window")
                ("Q" save-buffers-kill-emacs "Exit")))))

(use-package hl-todo            :config (global-hl-todo-mode)
  :custom (hl-todo--regexp "\\(\\<\\(TODO\\|HACK\\|CITE\\|IMAGE\\|LINK\\|FIXME\\)\\>\\)")
          (hl-todo-keyword-faces '(("HACK" . "#d0bf8f")
                                   ("FIXME" . "#cc9393")
                                   ("TODO" . "#cc9393")
                                   ("CITE" . "#dc8cc3")
                                   ("IMAGE" . "#dc8cc3")
                                   ("LINK" . "#dc8cc3"))))

(use-package org-modern
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t) (shell . t)))
  (require 'oc-biblatex)
  (defun toggle-org-pdf-export-on-save () (interactive)
         (if (memq 'org-latex-export-to-pdf after-save-hook)
             (progn
               (remove-hook 'after-save-hook 'org-latex-export-to-pdf t)
               (message "Disabled org latex export on save for current buffer..."))
           (add-hook 'after-save-hook 'org-latex-export-to-pdf nil t)
           (message "Enabled org latex export on save for current buffer...")))
  :custom
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
  :hook (org-mode . org-modern-mode)
        (org-mode . hl-todo-mode)
        (org-mode . visual-line-mode)
  :mode-hydra
  (org-mode
   (:exit t)
   ("Editing"  (("b" org-insert-structure-template)
                ("c" org-cite-insert)
                ("e" org-babel-execute-src-block)
                ("s" org-cut-subtree)
                ("t" org-time-stamp)
                ("T" (insert (format-time-string "%Y-%m-%d")) "today"))
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
                ("x" org-export-dispatch)))))
