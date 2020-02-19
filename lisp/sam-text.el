;;; sam-text.el --- text deriving editing            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: text

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

;; Code for editing text related stuff, like markdown.

;;; Code:


(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :hook ((markdown-mode . outline-minor-mode)
         (markdown-mode . abbrev-mode))
  :init
  (add-hook! 'mardown-mode-hook
    (remove-local-hook 'before-save-hook 'delete-trailing-whitespace)
    (setq-local before-save-hook nil)
    (add-local-hook 'after-save-hook #'sam-pandoc-render)))

(use-package adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode))

(defun sam-pandoc--args ()
  (format "--katex
--section-divs
--from=markdown
--filter=pandoc-citeproc
--filter=pandoc-sidenote
--bibliography=/Users/samuelbarreto/Dropbox/bibliography/references.bib
--csl=/Users/samuelbarreto/Dropbox/bibliography/styles/chicago-note-bibliography-with-ibid.csl
--to=html5
--template=/Users/samuelbarreto/blog/bacterial-finches/template/html/tufte.html5
--css=/Users/samuelbarreto/blog/bacterial-finches/static/css/latex.css
--css=/Users/samuelbarreto/blog/bacterial-finches/static/css/pandoc-solarized.css
--css=/Users/samuelbarreto/blog/bacterial-finches/static/css/pandoc.css
--css=/Users/samuelbarreto/blog/bacterial-finches/static/css/tufte-extra.css
--css=/Users/samuelbarreto/blog/bacterial-finches/static/css/tufte.css
--standalone
--output=%s.html
%s"
          (file-name-base) (buffer-file-name)))

(defun sam-pandoc--pdf-args ()
  (format "\
--template=/Users/samuelbarreto/dotfile/templates/pandoc/blog.latex
--filter pandoc-citeproc
--bibliography=/Users/samuelbarreto/Dropbox/bibliography/references.bib
--csl=/Users/samuelbarreto/Dropbox/bibliography/styles/chicago-note-bibliography-with-ibid.csl
--output %s.pdf
%s" (file-name-base) (buffer-file-name)))

(defvar sam-pandoc--proc nil)

(defun sam--pandoc ()
  (apply #'start-process
         `("pandoc" "*pandoc-render*"
           "pandoc"
           ,@(let ((split-string-default-separators " \\|\n\\|="))
               (split-string (sam-pandoc--args)))))
  (apply #'start-process
         `("pandoc" "*pandoc-render*"
           "pandoc"
           ,@(let ((split-string-default-separators " \\|\n\\|="))
               (split-string (sam-pandoc--pdf-args))))))

(defun sam-pandoc-render ()
  (interactive)
  (sam--pandoc))

(defun add-local-hook (hook function &optional append)
  "Add FUNCTION to HOOK, modifying its local value."
  (add-hook hook function append t))

(defun remove-local-hook (hook function)
  "Remove FUNCTION from HOOK locally"
  (remove-hook hook function t))



(provide 'sam-text)
;;; sam-text.el ends here
