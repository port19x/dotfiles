;;; use-package bootstrapping ;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(eval-after-load 'gnutls '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem")) ;; axe from here (in v29)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(eval-when-compile (require 'use-package)) 
(require 'bind-key) ;; too here
(setq use-package-always-ensure t)

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
(use-package doom-themes         :init    (load-theme 'doom-nord-aurora t))
(use-package doom-modeline       :init    (doom-modeline-mode))

;;; completion ;;;
(use-package vertico             :custom  (vertico-resize t)
                                 :init    (vertico-mode))
(use-package marginalia          :init    (marginalia-mode))
(use-package which-key           :init    (which-key-mode)
                                 :custom  (which-key-max-display-columns 3))
(use-package projectile          :init    (projectile-mode +1))
(use-package corfu               :custom  (corfu-auto t)
                                 :init    (global-corfu-mode))

;;; evil keys ;;;
(use-package evil                :init    (setq evil-want-keybinding nil)
                                 :config  (evil-mode 1)
                                 :custom  (evil-undo-system 'undo-redo))
(use-package evil-goggles        :init    (evil-goggles-mode))
(use-package evil-vimish-fold    :init    (global-evil-vimish-fold-mode))
(use-package evil-collection     :init    (evil-collection-init))

;;; clojure ;;;
;;; TODO bind several cider keys to the cider keymap
;; clojure treesitter
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
(use-package hl-todo             :init    (global-hl-todo-mode))
(use-package git-gutter          :init    (global-git-gutter-mode))
(use-package markdown-mode       :mode    "\\.md\\'")
(use-package lua-mode            :mode    "\\.lua\\'")
(use-package ansible             :mode    "\\.ya?ml\\'")

;;; media related stuff ;;;
(use-package org-re-reveal       :after   (org-mode)) ;TODO org-present
(use-package org-superstar       :hook    (org-mode . org-superstar-mode))
(use-package pdf-tools           :magic   ("%PDF" . pdf-view-mode)
                                 :config  (pdf-tools-install :no-query))
;(use-package nov                 :mode    "\\.epub\\'")

;;; TODO: space based global binds: (see email, general.el)
(use-package helpful             :bind    (([remap describe-function] . helpful-callable)
					   ([remap describe-variable] . helpful-variable)
					   ([remap describe-mode] . helpful-mode)
					   ([remap describe-key] . helpful-key)
					   ("C-h K" . describe-keymap)
					   ([remap describe-symbol] . helpful-symbol)
					   ([remap view-hello-file] . helpful-at-point)))
;SPC-h-f helpful describe function
;SPC-f-f find-file
;SPC-b-b ibuffer
;; TODO :init -> :config
;; TODO (info "(elisp) Key Binding Conventions") 
