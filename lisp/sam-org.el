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
  :commands (org-link-make-string
             org-next-visible-heading
             org-previous-visible-heading
             org-display-inline-images
             org-remove-inline-images
             org-toggle-inline-images
             org-get-buffer-tags)
  :functions (org-read-date)
  :mode (("\\.org\\'" . org-mode)
         ("README\\'"   . org-mode))
  :bind* (("C-c C-w" . org-refile)
          ("C-c C-l" . org-store-link)
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
  (org-archive-location ".archive_%s::")
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
  (org-tags-column 20)
  (org-hide-block-startup t)
  (org-refile-targets
   '(("~/these/meta/nb/These.org"         :level . 2)
     ("~/Org/TODO"                        :level . 1)
     ("~/Maths/maths.org"                 :level . 1)
     ("~/Logic/logic.org"                 :level . 1)
     ("~/blog/bacterial-finches/blog.org" :level . 1)))
  (org-default-notes-file "~/Org/notes.org")

  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-enforce-todo-dependencies t)
  (org-link-abbrev-alist
   '(("gsc"  . "https://scholar.google.com/scholar?q=%h")
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
  (org-startup-indented t)
  (org-image-actual-width '(300))
  :config
  (require 'org-datetree)

  (defun sam-org-heading-have-paper ()
    (interactive)
    (org-set-property "PRINTED" "yes"))

  (defun org-table-convert-to-latex ()
    (interactive)
    (unless (org-at-table-p)
      (user-error "Point is not in an org table"))
    (let ((end (org-table-end))
          (lt (orgtbl-to-latex
               (org-table-to-lisp
                (buffer-substring-no-properties
                 (org-table-begin) (org-table-end)))
               nil)))
      (goto-char end)
      (insert lt)))

  (defun sam-org-refile-to-archive-datetree (&optional bfn)
    "Refile an entry to a datetree under an archive."
    (interactive)
    (cl-flet
        ((org-read-datetree-date
          (d)
          (let ((dtmp (nthcdr 3 (parse-time-string d))))
            (list (cadr dtmp) (car dtmp) (caddr dtmp)))))
      (let*
          ((bfn (or bfn (find-file-noselect (expand-file-name "~/Org/journal.org"))))
           (datetree-date (org-read-datetree-date
                           (org-read-date t nil))))
        (org-refile
         nil nil
         (list nil (buffer-file-name bfn) nil
               (with-current-buffer bfn
                 (save-excursion
                   (org-datetree-find-date-create datetree-date)
                   (point))))
         "Done.")))
    (setq this-command 'sam-org-refile-to-journal))

  (require 'org-mac-link)
  (setq org-modules '(org-crypt))

  (defhydra hydra-org-archive (:color pink :columns 1)
    ("a" org-archive-subtree "archive")
    ("n" org-next-visible-heading "next")
    ("t" org-next-visible-heading "next")
    ("p" org-previous-visible-heading "previous")
    ("s" org-previous-visible-heading "previous")
    ("q" nil "quit"))

  (defhydra hydra-org-image (:color red :hint nil)
    "
Display inline images ?
_y_es  _n_o    _t_oggle
"
    ("y" org-display-inline-images)
    ("n" org-remove-inline-images)
    ("t" org-toggle-inline-images))

  (defun sam-org-set-tags (tag)
    (let ((ct (org-get-tags)))
      (if (member tag ct)
          (org-set-tags
           (cl-remove tag ct :test #'string-equal))
        (org-set-tags
         (cl-union ct (list tag)
                   :test #'string-equal)))))

  (defvar sam-biblio-tags '(("i" . "philo")
                            ("s" . "socio")
                            ("b" . "bio")
                            ("h" . "histoire")
                            ("r" . "roman")
                            ("e" . "pedagogie")
                            ("o" . "politique")))

  (defmacro sam-hydra-biblio (name &rest args)
    `(progn
       (defhydra ,name (,@args)
         ,@(mapcar
            (lambda (x) `(,(car x)
                          (lambda () (interactive)
                            (sam-org-set-tags ,(cdr x)))
                          ,(cdr x)))
            sam-biblio-tags)
         ("n" #'org-next-visible-heading "next")
         ("p" #'org-previous-visible-heading "previous")
         ("q" nil "quit"))))

  (defmacro sam-org-biblio--wrap (&rest body)
    `(unwind-protect
         ,@body
       (call-interactively #'sam-org-biblio-tag)))

  (let ()
    (defun sam-org-biblio--populate (prefix)
      (interactive
       (list (transient-args 'sam-org-biblio-tag)))
      (sam-org-biblio--wrap
       (mapcar 'sam-org-set-tags prefix)))

  (define-transient-command sam-org-biblio-tag ()
    "Main interface for spin scripts."
    ["Tagging"
     :class transient-columns
     [("i" "philo"  "philo")
      ("s" "socio"  "socio")
      ("É" "économie" "éco")
      ("h" "histoire"  "histoire")
      ("g" "linguistique" "linguistique")]
     [("b" "bio"  "bio")
      ("e" "pedagogie"  "pedagogie")
      ("o" "politique"  "politique")
      ("a" "anthropo" "anthropo")
      ("P" "physique" "physique")]
     [("r" "roman"  "roman")
      ("é" "poésie" "poésie")
      ("u" "musique" "musique")]]
    ["Status"
     ("lu" "lu" "lu")
     ("là" "à lire" "lire")
     ("lp" "prêté" "prêté")
     ("le" "emprunté" "emprunté")
     ("lc" "commandé" "commandé")
     ("lC" "à commander" "commander")]
    ["Actions"
     ("t" "tag" sam-org-biblio--populate)
     ("f" "filter" sam-org-biblio--filter)]
    ["Navigate"
     ("p" "previous" sam-org-previous-visible-heading)
     ("n" "next" sam-org-next-visible-heading)])

  (defun sam-org-previous-visible-heading ()
    (interactive)
    (sam-org-biblio--wrap (org-previous-visible-heading 1)))

  (defun sam-org-next-visible-heading ()
    (interactive)
    (sam-org-biblio--wrap (org-next-visible-heading 1)))

  (defun sam-org-biblio--filter (tags)
    (interactive
     (list (transient-args 'sam-org-biblio-tag)))
    (let ((match (mapconcat (lambda (x) (concat "+" x)) tags "")))
      (unwind-protect
          (org-match-sparse-tree nil match)
        (call-interactively #'sam-org-biblio-tag)))))


  (sam-hydra-biblio hydra-org-biblio :columns 2 :color pink)

  (defun org-babel-tangle-all-block-same-file ()
    "tangle all blocks which belong to the same file."
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively #'org-babel-tangle))))


(use-package org-agenda
  :bind* (("C-c a" . org-agenda))
  :custom (org-agenda-window-setup 'only-window)
  :config

  (defvar sam-org-agenda-files "~/.config/emacs/data/org-agenda-files.txt")
  (setq org-agenda-files (mapcar #'expand-file-name (sam--read-table sam-org-agenda-files)))

  (defun sam-org-add-to-agenda-files ()
    (interactive)
    (let ((f (buffer-file-name))
          (fl org-agenda-files)
          (oaf sam-org-agenda-files))
      (if (and (eq major-mode 'org-mode)
               (not (member f fl)))
        (sam-append-string-to-file f oaf)
        (add-to-list org-agenda-files f))))

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
          ("l" "Lab" alltodo ""
           ((org-agenda-files
             '("~/these/meta/nb/These.org"
               "~/these/projets/papers/2018-09-17-experimental-evidence/analysis/TODO"
               "~/these/projets/papers/2018-09-17-experimental-evidence/TODO"
               "~/these/projets/papers/2018-09-17-experimental-evidence/figures/tables/TODO"
               "~/these/projets/papers/2018-09-17-experimental-evidence/figures/TODO"))
            (org-agenda-sorting-strategy '(timestamp-up priority-up)))
           ("~/these/meta/nb/These.html"))
          ("p" "perso" tags "@perso"
           ((org-agenda-sorting-strategy '(ts-up priority-up))))))

  (setq org-agenda-include-diary nil))

(use-package org-archive
  :after org
  :commands (org-archive-subtree))


;; (use-package org-bullets
;;   :hook
;;   (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "◉" "○" "◉" "○")))


(use-package org-capture
  :bind* (("C-M-c" . org-capture))
  :custom
  (org-capture-templates
   '(("t" "THESE")
     ("tt" "these - Todo"     entry (file+headline "~/these/meta/nb/These.org" "InBox"             ) "** %?\n%U")
     ("tl" "these - Lab"      entry (file+olp+datetree "~/these/meta/nb/journal.org"               ) "* %(hour-minute-timestamp) %?%^g\n")
     ("td" "these - tickleR"  entry (file+headline "~/these/meta/nb/These.org" "Tickler"           ) "** %?\n%T")
     ("tr" "these - rapport"  entry (file+headline "~/these/these/notes-manuscrit.org" "Collecte"  ) "** %?\n%U")
     ("p" "PERSO")
     ("pt" "perso - Collecte" entry (file+headline "~/Org/TODO" "Collecte"                         ) "** %?\n%U\n")
     ("pn" "perso - Notes"    entry (file+olp+datetree "~/Org/journal.org"                         ) "* %(hour-minute-timestamp) %?%^g\n")
     ("c" "collecte" entry (file+headline "~/Org/TODO" "Collecte"                         ) "** %?\n%U\n")
     ("n" "journal" entry (file+olp+datetree "~/Org/journal.org"                     ) "* %(hour-minute-timestamp) %?\n")
     ("h" "horizon" entry (file+olp+datetree "~/Org/journal.org"                     ) "* %(hour-minute-timestamp) %? :horizon:\n"))))


(use-package org-download
  :after org
  :bind* (:map org-mode-map
          ("C-c y e" . org-download-edit)
          ("C-c y e" . org-download-edit)
          ("C-c y i" . org-download-image)
          ("C-c y s" . org-download-screenshot)
          ("C-c y y" . org-download-yank)
          ("C-c y k" . org-download-delete))
  :init
  (defvar sam-org-download-dir "./img/"
    "Default folder to place `org-download' captures in.")
  (when (eq system-type 'darwin)
    (setq-default org-download-screenshot-method "screencapture -i %s"))

  (defun sam-img-dir ()
    (let (target sam-org-download-dir)
      (cond ((file-directory-p target) target)
            (t (make-directory target) target))))
  (setq org-download-image-dir sam-org-download-dir)
  (setq org-download-heading-lvl nil))


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

(use-package ox-md
  :commands (org-md-export-as-markdown
             org-md-export-to-markdown))

(defmacro use-ox-package (pkg)
  `(use-package ,pkg :after ox))

(defmacro use-ox-packages (&rest pkgs)
  `(progn ,@(mapcar (lambda (p) `(use-ox-package ,p)) pkgs)))

(use-ox-packages ox-gfm ox-pandoc ox-opml)

(use-package ox-latex
  :commands (org-latex-export-as-latex
             org-latex-export-to-latex
             org-latex-export-to-pdf)
  :config

  (define-after-save-hook-mode sam-ox-latex #'org-latex-export-to-pdf
    "autoexport"
    "Export org buffer when saving")

  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

  (setq
   ;; moyen d'export latex
   org-latex-pdf-process
   (list "latexmk -f %f")
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

  (setq
   org-latex-classes
   '(("tant"
      "\\documentclass[oneside,a4paper,10pt]{tant}
\\addbibresource{/Users/samuelbarreto/Dropbox/bibliography/references.bib}
"
      ;; ("\\part{%s}" . "\\part*{%s}")
      ;; ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section*{%s}" . "\\section*{%s}")
      ("\\subsection*{%s}" . "\\subsection*{%s}"))
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


(use-package org-mind-map
  :after ox-org
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

(use-package org-mu4e
  :after org)

(use-package org-clock
  :after org
  :bind* ("C-þ" . #'hydra-org-clock/body)
  :config
  (defhydra hydra-org-clock (:color blue :columns 1)
    "CLOCK"
    ("i" #'org-clock-in "in")
    ("o" #'org-clock-out "out")
    ("g" #'org-clock-goto "goto")))



(use-package counsel-recoll
  :commands (counsel-recoll))


(defconst sam-org-notes-directory (expand-file-name "~/notes")
  "Path to my note taking directory.")

(use-package org-roam
  :after org
  :hook (org-mode . org-roam-mode)
  :custom
  (org-roam-directory sam-org-notes-directory)
  :bind*
  (("C-c n r" . org-roam)
   ("C-c n f" . org-roam-find-file)
   ("C-c n i" . org-roam-insert)
   ("C-c n g" . org-roam-show-graph)))

(use-package deft
  :bind* (("C-c n d" . #'deft))
  :custom
  (deft-auto-save-interval 60.0)
  (deft-directory sam-org-notes-directory)
  (deft-recursive t)
  (deft-default-extension "org")
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t))

(use-package org-journal
  :bind*
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-enable-agenda-integration t)
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-time-prefix "* ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir sam-org-notes-directory)
  (org-journal-date-format "%A, %d %B %Y")
  :config
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))

  (add-to-list 'org-capture-templates
               '("e" "Journal entry" entry (function org-journal-find-location)
                 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))

(defhydra sam-notes (:color blue :hint nil)
  "
NOTES
--------------------------------------------

^[_r_]oam^:      ^browse^    ^journal^
[_f_]ind file  [_d_]eft    [_j_]ournal entry
[_i_]nsert
[_g_]graph"
  ("r" #'org-roam)
  ("f" #'org-roam-find-file)
  ("i" #'org-roam-insert)
  ("g" #'org-roam-show-graph)
  ("d" #'deft)
  ("j" #'org-journal-new-entry))
(bind-key* "s-e" #'sam-notes/body)


(provide 'sam-org)
;;; sam-org.el ends here
