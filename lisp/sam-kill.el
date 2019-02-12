;;; sam-kill.el --- kill mode for killing impromptu buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: kill

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

(defgroup kill nil
  "The kill group."
  :prefix "kill-"
  :group 'kill)

(defvar kill-keymap (make-sparse-keymap))

(define-key kill-keymap (kbd "q") #'kill-this-buffer)

(define-minor-mode kill-minor-mode
  "A simple minor mode that kills the current buffer."
  :keymap kill-keymap)

(defmacro kill-enable-for-buffers (&rest args)
  "Enable `kill-minor-mode' for all buffers that match a regexp in ARGS.

ARGS are added to `auto-mode-alist'."
  (declare (indent 0))
  `(progn
     ,@(mapcar (lambda (re) `(add-to-list 'auto-mode-alist '(,re . kill-minor-mode)))
               args)))

(kill-enable-for-buffers
  "\\*\\(?:\\|Async Shell Command\\|Compile-Log\\)\\*")

(provide 'sam-kill)
;;; sam-kill.el ends here
