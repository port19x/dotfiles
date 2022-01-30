;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "ura"
      user-mail-address "ura@ura43.xyz")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Mononoki" :size 32)
      doom-big-font (font-spec :family "Mononoki" :size 64))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-oceanic-next)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;fullscreen startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(require 'elcord)
(elcord-mode)
(require 'org-bullets)
(pdf-loader-install)
;;

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;Prettify symbols in org mode
(add-hook
 'org-mode-hook
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(;; Set Theory
           ("in" .              #x2208)
           ("not in" .          #x2209)
           ("part" .            #x2286)
           ("rpart" .           #x2282)
           ("union" .           #x222A)
           ("bisect" .          #x2229)
           ("sym" .             #x2206)
           ("{}" .              #x2205)
           ;; Number Sets
           ("BigP" .            #x2119)
           ("BigN" .            #x2115)
           ("BigZ" .            #x2124)
           ("BigQ" .            #x211A)
           ("BigR" .            #x211D)
           ;; Quantors
           ("for all" .         #x2200)
           ("exists" .          #x2203)
           ("none exist" .      #x2204)
           ;; Logic
           ("not" .             #xAC)
           ("and" .             #x2227)
           ("or" .              #x2228)
           ("implies" .         #x27F9)
           ("equals" .          #x27FA)
           ))))
