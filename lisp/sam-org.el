;;; sam-org.el --- org customisations                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:




(use-package org
  :commands (org-make-link-string
             org-next-visible-heading
             org-previous-visible-heading
             org-display-inline-images
             org-remove-inline-images
             org-toggle-inline-images
             org-get-buffer-tags)
  :mode (("\\.org\\'" . org-mode)
         ("README\\'"   . org-mode))
  :bind* (("C-c C-w" . org-refile)
          ("C-c C-l" . org-store-link)
          ("C-M-c"   . org-capture)
          :map org-mode-map
          ("s-e"     . org-babel-tangle-all-block-same-file)
          ("s-l"     . org-latex-export-to-latex)
          ("C-c ."   . org-time-stamp)
          ("C-c M-i" . org-insert-link)
          ("C-c m"   . hydra-org-image/body)
          ("C-c $"   . hydra-org-archive/body)
          ("C-c e d" . org-decrypt-entry)
          ("C-c e e" . org-encrypt-entry)
          ("C-c e s" . org-sparse-tree)
          ("C-c e t" . org-tags-sparse-tree))
  :custom
  ;; activate speed commands when on any outline star
  (org-use-speed-commands
   (lambda () (and (looking-at org-outline-regexp)
              (looking-back "^\**" (1- (point))))))
  (org-indirect-buffer-display 'current-window)
  ;; make symlink instead of hard copy
  (org-attach-method 'lns)
  ;; delete attachment when archiving entry
  (org-attach-archive-delete t)
  ;; change folder from data/ to .attach/
  (org-attach-directory ".attach/")
  (org-tags-column 80)
  (org-hide-block-startup t)
  (org-refile-targets '(("~/these/meta/nb/These.org" :level . 2)
                        ("~/Org/TODO" :level . 2)
                        ("~/Org/TODO" :level . 1)
                        ("~/these/meta/nb/maybe.org" :level . 1)
                        ("~/Org/maybe.org" :level . 1)))
  (org-default-notes-file "~/Org/notes.org")

  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-enforce-todo-dependencies t)
  (org-link-abbrev-alist
   '(("wiki" . "https://en.wikipedia.org/wiki/%h")
     ("gsc"  . "https://scholar.google.com/scholar?q=%h")
     ("sep"  . "https://plato.stanford.edu/search/search?query=%h")
     ("etym" . "http://www.cnrtl.fr/etymologie/%h")
     ("bu"   . "http://hola.univ-lyon1.fr/ipac20/ipac.jsp?menu=search&aspect=basic_search&npp=10&ipp=20&spp=20&profile=scd&ri=&index=.GK&term=%h&terms=&valider=Ok")))
  (org-crypt-disable-auto-save t)
  (org-src-preserve-indentation t)
  (org-footnote-auto-adjust t)
  (org-footnote-define-inline nil)
  (org-footnote-fill-after-inline-note-extraction t)
  (org-footnote-section nil)
  (org-export-with-todo-keywords nil)
  (org-export-default-language "fr")
  (org-export-backends '(ascii html icalendar latex md))
  (org-export-with-tags nil)
  (org-startup-with-inline-images t)
  (org-startup-indented nil)
  (org-image-actual-width '(400))
  :config

  (setq org-modules '(org-crypt))

  (defhydra hydra-org-archive (:color red :columns 1)
    ("a" org-archive-subtree "archive")
    ("n" org-next-visible-heading "next")
    ("t" org-next-visible-heading "next")
    ("p" org-previous-visible-heading "previous")
    ("s" org-previous-visible-heading "previous"))

  (defhydra hydra-org-image (:color red :hint nil)
    "
Display inline images ?
_y_es  _n_o    _t_oggle
"
    ("y" org-display-inline-images)
    ("n" org-remove-inline-images)
    ("t" org-toggle-inline-images))

  (defun org-babel-tangle-all-block-same-file ()
    "tangle all blocks which belong to the same file."
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively #'org-babel-tangle))))


(use-package org-agenda
  :bind* (("C-c a" . org-agenda))
  :custom (org-agenda-window-setup 'current-window)
  :config

  ;; inspired from  http://pages.sachachua.com/.emacs.d/Sacha.html#orgce6f46d
  (setq org-agenda-files
        (list "~/Org/TODO"
              "~/these/meta/nb/These.org"))

  (setq org-capture-use-agenda-date t) ; when press k from agenda, use agenda date.
  (setq org-agenda-span 7)
  (setq org-agenda-tags-column -100) ; take advantage of the screen width
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-deadline-warning-days 4)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

  ;; agenda start on mondays
  (setq org-agenda-start-on-weekday 1)

  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((agenda "")
            (alltodo "")))
          ("l" "Lab" tags-todo "@these"
           ((org-agenda-files '("~/these/meta/nb/These.org"))
            (org-agenda-sorting-strategy '(timestamp-up priority-up)))
           ("~/these/meta/nb/These.html"))
          ("p" "perso" tags "@perso"
           ((org-agenda-sorting-strategy '(ts-up priority-up))))))

  (setq org-agenda-include-diary nil))


(use-package org-archive
  :after org
  :commands (org-archive-subtree))


(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "◉" "○" "◉" "○")))


(use-package org-capture
  :bind* (("C-M-c" . org-capture))
  :custom
  (org-capture-templates
   '(("t" "these - Todo"     entry (file+headline "~/these/meta/nb/These.org" "InBox"             ) "** %?\n%U")
     ("l" "these - Lab"      entry (file+olp+datetree "~/these/meta/nb/journal.org"               ) "* %(hour-minute-timestamp) %?%^g\n")
     ("r" "these - tickleR"  entry (file+headline "~/these/meta/nb/These.org" "Tickler"           ) "** %?\n%T")
     ("a" "these - rapport"  entry (file+headline "~/these/these/notes-manuscrit.org" "Collecte"  ) "** %?\n%U")
     ("c" "perso - Collecte" entry (file+headline "~/Org/TODO" "Collecte"                         ) "** %?\n%U\n")
     ("n" "perso - Notes"    entry (file+olp+datetree "~/Org/journal.org"                         ) "* %(hour-minute-timestamp) %?%^g\n"))))


(use-package org-download
  :after org
  :bind* (:map org-mode-map
               ("C-c y e" . org-download-edit)
               ("C-c y e" . org-download-edit)
               ("C-c y i" . org-download-image)
               ("C-c y s" . org-download-screenshot)
               ("C-c y y" . org-download-yank)
               ("C-c y k" . org-download-delete))
  :config
  (defvar sam-org-download-dir "./img/"
    "Default folder to place `org-download' captures in.")

  (defun sam-img-dir ()
    (let (target sam-org-download-dir)
      (cond ((file-directory-p target) target)
            (t (make-directory target) target))))

  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir sam-org-download-dir)
  (when (eq system-type 'darwin)
    (setq-default org-download-screenshot-method "screencapture -i %s")))


(use-package org-web-tools
  :commands (org-web-tools-insert-link-for-url
             org-web-tools-insert-web-page-as-entry
             org-web-tools-read-url-as-org
             org-web-tools-convert-links-to-page-entries))


(use-package ob
  :after org
  :commands (org-babel-tangle))


(use-package ob-R
  :commands (org-babel-execute:R))

(use-package ob-perl
  :commands (org-babel-execute:perl))

(use-package ob-python
  :commands (org-babel-execute:python))

(use-package ob-shell
  :commands (org-babel-execute:shell))

(use-package ob-emacs-lisp
  :commands (org-babel-execute:emacs-lisp))

(use-package ob-dot
  :commands (org-babel-execute:dot))

(use-package ob-makefile
  :commands (org-babel-execute:makefile))

(use-package ox-latex
  :commands (org-latex-export-as-latex
             org-latex-export-to-latex)
  :config

  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

  (setq
   ;; moyen d'export latex
   org-latex-pdf-process
   (list "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         "biber %f"
         "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f")
   org-latex-image-default-width "1\\linewidth"
   org-highlight-latex-and-related '(latex entities) ; colore les macro LaTeX
   ;; tufte-handout class by default.
   org-latex-default-class "tant"
   ;; default package list with sensible options
   org-latex-default-packages-alist nil
   org-latex-listings-langs
   '((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp") (c "C") (cc "C++") (fortran "fortran")
     (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby") (html "HTML")
     (xml "XML") (tex "TeX") (latex "[LaTeX]TeX") (shell-script "bash") (gnuplot "Gnuplot")
     (ocaml "Caml") (caml "Caml") (sql "SQL") (sqlite "sql") (makefile "make")
     (R "r"))
   ;; files extensions that org considers as latex byproducts.
   org-latex-logfiles-extensions
   '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx"
     "log" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv" "bbl")
   org-latex-minted-langs '((emacs-lisp "common-lisp")
                            (cc "c++")
                            (cperl "perl")
                            (shell-script "bash")
                            (caml "ocaml")
                            (python "python")
                            (ess "R"))
   org-latex-remove-logfiles t
   org-src-fontify-natively t
   org-latex-tables-booktabs t)

  (append-to-list!
   'org-latex-classes
   '(("tant"
      "\\documentclass[twoside,a4paper,10pt]{tant}
% \\addbibresource{reference.bib}
"
      ;; ("\\part{%s}" . "\\part*{%s}")
      ;; ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("tufte-book"
      "\\documentclass[a4paper, sfsidenotes, justified, notitlepage]{tufte-book}
       \\input{/Users/samuelbarreto/.templates/tufte-book.tex}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("tufte-handout"
      "\\documentclass[a4paper, justified]{tufte-handout}
       \\input{/Users/samuelbarreto/.templates/tufte-handout.tex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("rapport" "\\documentclass[11pt, oneside]{scrartcl}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass[presentation]{beamer}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("journal"
      "\\documentclass[9pt, oneside, twocolumn]{scrartcl}
       \\input{/Users/samuelbarreto/.templates/journal.tex}"
      ("\\part{%s}" . "\\section*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")))))

(use-package ox-beamer
  :commands (org-beamer-mode))


(provide 'sam-org)
;;; sam-org.el ends here
