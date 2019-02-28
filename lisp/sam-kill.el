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

;; TODO: [2019-02-14 09:57] doesn't work for now.

;;; Code:

(defgroup kill nil
  "The kill group."
  :prefix "kill-"
  :group 'kill)

(defvar kill-keymap (make-sparse-keymap))

(define-key kill-keymap (kbd "q") #'kill-this-buffer)

(defcustom kill-buffers
  '("\\*\\(?:\\|Async Shell Command\\|Compile-Log\\)\\*"
    "\\*Pp Macroexpand Output\\*")
  "List of regexp that should enable the `kill-minor-mode'."
  :type 'list
  :group 'kill)

(define-minor-mode kill-minor-mode
  "A simple minor mode that kills the current buffer."
  :keymap kill-keymap)

(defun kill-enable-for-buffers (&rest args)
  "Enable `kill-minor-mode' for all buffers that match a regexp in ARGS.

ARGS are added to `kill-buffers'."
  (declare (indent 0))
  (setq kill-buffers (nconc args kill-buffers)))

(defun kill--buffer-p (bufname)
  (cl-find-if
   (lambda (x) (string-match x bufname))
   kill-buffers))

(defun kill--toggle ()
  (when (kill--buffer-p (buffer-name))
    (kill-minor-mode 1)))

(advice-add #'set-auto-mode :after #'kill--toggle)

(provide 'sam-kill)
;;; sam-kill.el ends here
