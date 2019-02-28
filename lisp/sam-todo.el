;;; sam-todo.el --- todo                             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: todo

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

(require 'sam-keybindings)
(require 'sam-utils)
(require 'calendar)
;; (require 'todo-mode)

(sam-set-custom
  calendar-date-style 'iso)

(defun sam-todo ()
  (interactive)
  (let ((buf (find-file-noselect "TODO")))
    (with-current-buffer buf
      (org-mode)
      (setq-local fill-column 42)
      (text-scale-decrease 2))
    (sam-side-buffer buf
        `((side          . right)
          (slot          . 1)
          (window-width  . fit-window-to-buffer)
          (preserve-size . (t . nil)) ,sam--parameters))))

(defun sam-todo-insert ()
  (interactive)
  (sam-todo)
  (goto-char (point-max))
  (call-interactively #'org-insert-todo-heading))

(sam-defkeys
  "C-c c" 'calendar
  "C-c d" 'diary
  "C-c t" 'sam-todo
  "C-c i" 'sam-todo-insert)

(provide 'sam-todo)
;;; sam-todo.el ends here
