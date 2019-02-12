;;; sam-autoinsert.el --- definitions for auto-insert  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: autotype, autoinsert

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

(require 'autoinsert)

(defmacro sam-autoinsert (&rest args)
  "Add ARGS to `auto-insert-alist'"
  `(progn ,@(mapcar (lambda (arg) `(add-to-list 'auto-insert-alist ,arg)) args)))

;; insert header for R script.
(sam-autoinsert
 '(("\\.R\\'" . "R Header")
   "Short Description: "
   "### " (file-name-nondirectory (buffer-file-name)) " --- " str

   "

## Copyright (C) " (format-time-string "%Y") "  " user-full-name
   "

## Author:  " user-full-name
   "
## License: GPL3+ (see <https://www.gnu.org/licenses/gpl-3.0.txt>)
## Time-stamp: <>"
   "

## * Commentary

#'
#'

## * Code

## ** Libraries



## " (file-name-nondirectory (buffer-file-name)) " ends here."))

;; insert header for org mode todos.
(sam-autoinsert
 '(("TODO\\'" . "TODO Header") ""
   "# -*- mode: org -*-
# Time-stamp: <>
"))

(auto-insert-mode 1)



(provide 'sam-autoinsert)
;;; sam-autoinsert.el ends here
