;; General Settings
(setq doom-font (font-spec :family "iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "iosevka aile" :size 14)
      doom-big-font (font-spec :family "iosevka" :size 28)
      doom-theme 'doom-gruvbox
      doom-gruvbox-dark-variant "hard"
      user-full-name "port19"
      user-mail-address "port19@port19.xyz"
      display-line-numbers-type `relative)

;; Org UX
(require 'org-superstar)
(setq org-directory "~/doc/")
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(add-hook 'org-clock-in-hook (lambda () (org-timer-set-timer 25)))
(add-hook 'org-clock-out-hook (lambda () (org-timer-stop)))
(add-hook 'org-mode-hook (lambda () (delete '("\\.pdf\\'" . default) org-file-apps)
                                    (add-to-list 'org-file-apps '("\\.pdf\\'" . "mupdf %s"))))

;; Science Papers
(require 'org-ref)
(setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                              "bibtex %b"
                              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
      org-latex-logfiles-extensions (quote ("lof" "lot" "tex" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "pygtex" "pygstyle"))
      bibtex-completion-bibliography '("~/doc/praxisarbeit/t1000.bib")
      bibtex-completion-display-formats '((book        . "${year:4} ${author:36} ${title:*}") (online      . "${year:4} ${institution:36} ${title:*} ${url:*}"))
      bibtex-completion-library-path '("~/doc/praxisarbeit/")
      bibtex-completion-pdf-open-function (lambda (fpath) (call-process "mupdf" nil 0 nil fpath)))

;; Clojure UX
(map! :after cider-mode :map clojure-mode-map :n "," #'cider-eval-last-sexp)
