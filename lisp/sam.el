;;; sam.el --- personal package                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto
;; Time-stamp: <2019-11-17 17:09:53 samuelbarreto>

;; Version: 0.1
;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: conv

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

(defgroup sam nil
  "Personal function and keybindings"
  :prefix "sam-"
  :group 'lisp)

(require 'sam-autoinsert)
(require 'sam-cc-mode)
(require 'sam-compile)
(require 'sam-completion)
(require 'sam-defaults)
(require 'sam-dired)
(require 'sam-ess)
(require 'sam-helpers)
(require 'sam-keybindings)
(require 'sam-kill)
(require 'sam-latex)
(require 'sam-mail)
(require 'sam-news)
(require 'sam-org)
(require 'sam-prog)
(require 'sam-python)
(require 'sam-text)
(require 'sam-themes)
(require 'sam-todo)
(require 'sam-utils)
(require 'sam-viridis)

(require 'intcrement)

(provide 'sam)
;;; sam.el ends here
