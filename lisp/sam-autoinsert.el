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

(add-to-list
 'auto-insert-alist
 '(("\\.R\\'" . "R Header")
   "Short Description: "
   "### " (file-name-nondirectory (buffer-file-name)) " --- " str

   "

## Copyright (C) " (format-time-string "%Y") "  " user-full-name
   "

##  Author: " user-full-name
   "
## License: GPL3+ (see <https://www.gnu.org/licenses/gpl-3.0.txt>)"
   "

## * Commentary

#'
#'

## * Code

## ** Libraries



## " (file-name-nondirectory (buffer-file-name)) " ends here."))

(auto-insert-mode 1)



(provide 'sam-autoinsert)
;;; sam-autoinsert.el ends here
