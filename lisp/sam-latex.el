;;; sam-latex.el --- latex editing                   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: latex, tex

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

(eval-when-compile (require 'subr-x))

(use-package auctex
  :load tex-site
  :config
  (setq TeX-PDF-mode t)
  (setq TeX-engine 'default)
  (setq LaTeX-item-indent 2)
  (setq LaTeX-csquotes-close-quote "}")
  (setq LaTeX-csquotes-open-quote "\\enquote{")
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;; Set the list of viewers for Mac OS X.
  (setq TeX-view-program-list
        '(("Preview.app" "open -a Preview.app %o")
          ("Skim" "open -a Skim.app %o")
          ("displayline" "displayline %n %o %b")
          ("open" "open %o")))
  ;; Select the viewers for each file type.
  (setq TeX-view-program-selection
        '((output-dvi "open")
          (output-pdf "Skim")
          (output-html "open")))

  (setq-default TeX-master nil)


  (setq-default cdlatex-math-modify-prefix (kbd "'"))
  (require 'cdlatex)
  (add-to-hook 'LaTeX-mode-hook
    'turn-on-cdlatex
    'reftex-mode
    'hs-minor-mode)

  (use-package auctex-latexmk
    :commands (auctex-latexmk-setup)
    :config
    (auctex-latexmk-setup)
    (setq auctex-latexmk-inherit-TeX-PDF-mode t))

  (use-package reftex
    :hook (TeX-latex-mode . reftex-mode)
    :custom
    (reftex-default-bibliography
     '("/Users/samuelbarreto/Dropbox/bibliography/references.bib"))
    (reftex-cite-format 'biblatex)))



(defun sam-reftex-file ()
  (interactive)
  (progn
    (reftex-view-crossref 2)
    (bibtex-narrow-to-entry)
    (search-forward "file = {" nil t))
  (counsel-find-file-extern
   (concat (thing-at-point 'symbol t) ".pdf")))

; --------------------------------------------------------------- latexmk

(defvar sam-latexmk-process-name "latexmk"
  "default process for latexmk")

(defvar sam-latexmk-process nil
  "placeholder for latexmk process")

(defun sam-latexmk ()
  (interactive)
  (setq sam-latexmk-process
        (apply #'start-process
               `(,sam-latexmk-process-name
                 "*latexmk*" "latexmk" "-f" "-quiet" "-pvc"
                 ,(buffer-file-name)))))

(defun sam-latexmk-kill ()
  (interactive)
  (when-let ((p (processp sam-latexmk-process)))
    (kill-process p)
    (setq sam-latexmk-process nil)))

(defun delete-match (re)
  (save-excursion
    (search-forward re)
    (delete-region (match-beginning 0)
                   (match-end 0))))

(defun LaTeX-point-in-figure-p ()
  (save-excursion
    (let ((p (point))
          (beg (progn (LaTeX-find-matching-begin) (point)))
          (end (progn (LaTeX-find-matching-end) (point))))
      (and (< beg p)
           (< p end)))))

(defun sam-latex-figure-to-sidecaption ()
  (interactive)
  (let* ((beg (progn (LaTeX-find-matching-begin) (point)))
         (end (save-excursion (search-forward "\\end{figure}")))
         (labelcoord
          (save-excursion
            (list
             (search-forward "\\label{" end)
             (progn (up-list) (1- (point))))))
         (label (unwind-protect
                    (apply #'buffer-substring-no-properties labelcoord)
                  (apply #'delete-region labelcoord)))
         (capcoord
          (save-excursion
            (list
             (search-forward "\\caption{" end)
             (progn (up-list) (1- (point))))))
         (caption (unwind-protect
                      (apply #'buffer-substring-no-properties capcoord)
                    (apply #'delete-region capcoord))))
    (goto-char beg)
    (end-of-line)
    (insert
     (format "
\\begin{sidecaption}%%
  {%s}%%
  [%s]
"
             caption label))
    (search-forward "\\end{figure")
    (forward-line -1)
    (insert "
\\end{sidecaption}")

    (goto-char beg)
    (delete-match "\\caption{}")
    (delete-match "\\label{}")))

(use-package ebib
  :commands (ebib)
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-bib-search-dirs '("~/Dropbox/bibliography/"))
  (setq ebib-preload-bib-files '("~/Dropbox/bibliography/references.bib"))
  (setq ebib-file-associations '(("pdf" . "open")
                                 ("epub" . "open")
                                 ("djvu" . "open")))
  (ebib-init))

; ---------------------------------------------------------------- reftex




(provide 'sam-latex)
;;; sam-latex.el ends here
