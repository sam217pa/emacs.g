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

(defun sam-todo--file ()
  "Return the file name of current project TODO or the TODO buffer in current directory"
  (let ((todof (concat (car (project-roots (project-current))) "TODO")))
    (if (file-exists-p todof)
        todof
      "TODO")))

(defun sam-todo ()
  "Find the file listing stuff to do in the current directory."
  (interactive)
  (let ((buf (find-file-noselect
              (sam-todo--file))))
    (ignore-errors (org-store-link nil t))
    (with-current-buffer buf
      (org-mode)
      (setq-local visual-fill-column-width 42)
      (setq-local fill-column 42)
      (visual-fill-column-mode 1)
      (visual-line-mode 1)
      (face-remap-add-relative 'org-level-1 '(:height 100))
      (text-scale-set -3)
      (define-key (make-sparse-keymap) (kbd "C-c C-k") #'kill-this-buffer))
    (sam-side-buffer buf
        `((side          . right)
          (slot          . 0)
          (window-height . 10)
          (window-width  . 45)
          (preserve-size . (t . nil)) ,sam--parameters))))

(defun sam-todo-insert ()
  "Insert a new entry in the list of stuff to do."
  (interactive)
  (sam-todo)
  (goto-char (point-max))
  (call-interactively #'org-insert-todo-heading))

(defun sam-add-change-log ()
  "Add a new entry in the ChangeLog file in a side window."
  (interactive)
  (let ((buf (sam-change-log)))
    (with-current-buffer buf
      (add-change-log-entry nil "ChangeLog"))))

(defun sam-change-log ()
  "Show ChangeLog in a side window."
  (interactive)
  (let ((buf (find-file-noselect "ChangeLog")))
    (with-current-buffer buf
      (setq-local fill-column 52)
      (text-scale-set -4))
    (sam-side-buffer buf
        `((side          . right)
          (slot          . 1)
          (window-width  . fit-window-to-buffer)
          (preserve-size . (t . nil)) ,sam--parameters))
    buf))

(sam-defkeys
  "C-c c" 'calendar
  "C-c d" 'diary
  "C-c t" 'sam-todo
  "C-c i" 'sam-todo-insert
  "C-c l i" 'sam-add-change-log
  "C-c l l" 'sam-change-log)

(provide 'sam-todo)
;;; sam-todo.el ends here
