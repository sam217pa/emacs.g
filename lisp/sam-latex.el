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

;; this is the "hook" function
(defun sam-latex-dont-insert-expansion-char ()  t)
;; the hook should have a "no-self-insert"-property set
(put 'sam-latex-dont-insert-expansion-char 'no-self-insert t)

(define-abbrev-table 'latex-mode-abbrev-table
  '(("em" "\\emph{" sam-latex-dont-insert-expansion-char 0)))


(provide 'sam-latex)
;;; sam-latex.el ends here
