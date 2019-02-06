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
(require 'calendar)
(require 'todo-mode)

(sam-set-custom
  calendar-date-style 'iso)

(sam-defkeys
  "C-c c" 'calendar
  "C-c d" 'diary
  "C-c t" 'todo-show
  "C-c j" 'todo-jump-to-category
  "C-c i" 'todo-insert-item)

(provide 'sam-todo)
;;; sam-todo.el ends here
