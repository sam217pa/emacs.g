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

(defun sam-autoinsert (&rest args)
  (declare (indent 0))
  "Add ARGS to `auto-insert-alist'"
  (mapcar (lambda (it) (add-to-list 'auto-insert-alist it))
          args))

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



## * " (file-name-nondirectory (buffer-file-name)) " ends here.")

 '(("\\.r\\'" . "R spin") ""
   "#' ---
#' title: \"" (read-string "Document title: ") "\"
#' author: \"Samuel Barreto\"
#' date: \"`r Sys.Date()`\"
#' output: tint::tintPdf
#' latexfonts:
#' - package: mathdesign
#'   options:
#'     - bitstream-charter
#' - package: nimbusmononarrow
#' ---

#+ knitr, echo=FALSE
opts_chunk$set(echo=TRUE, prompt=FALSE, comment=NA, message=FALSE,
               warning=FALSE, global.par = TRUE, fig.path = \"figures/\",
               fig.width=7, fig.height=5)")

 ;; insert header for org mode todos.
 '(("TODO\\'" . "TODO Header") ""
   "# -*- mode: org -*-
# Time-stamp: <>
")
 '(("\\.md\\'" . "Md Header") ""
   "---
title: \"" (read-string "Post title: ") "\"
author: \"" (read-string "Post author: ") "\"
date: 
draft: true
---")) ; sam-autoinsert

;; enable auto-insert mode
(auto-insert-mode 1)

(provide 'sam-autoinsert)
;;; sam-autoinsert.el ends here
