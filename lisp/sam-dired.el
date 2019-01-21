;;; sam-dired.el --- personnal dired helpers         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: convenience, files, dired

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

(defun sam-dired--uuidgen ()
  (replace-regexp-in-string
   "\n\\'" ""
   (shell-command-to-string "uuidgen")))

;;;###autoload
(defun sam-dired-uuidgen (&optional arg)
  "Create a directory named by uuidgen, and add a ChangeLog entry
that describes it."
  (interactive "p")
  (let ((dir (sam-dired--uuidgen)))
    (dired-create-directory dir)
    (if arg
        (add-change-log-entry-other-window)
      (add-change-log-entry))
    (insert dir)))

(defmacro sam-dired-with-marked-files (file &rest body)
  (declare (indent 1))
  `(let ((files (dired-get-marked-files t)))
     (mapc
      (lambda (,file) ,@body)
      files)
    (dired-do-redisplay)))

(defun sam-dired-chmod-read-only ()
  "Change mode of marked files to read only."
  (interactive)
  (sam-dired-with-marked-files f
    (set-file-modes f (string-to-number "0444" 8))))

(provide 'sam-dired)
;;; sam-dired.el ends here
