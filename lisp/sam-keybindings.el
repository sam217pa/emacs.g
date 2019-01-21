;;; sam-keybindings.el --- personnal keybindings     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: keybindings, keys

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

(require 'bind-key)
(require 'sam-utils)

(defmacro sam-defkeys (&rest args)
  "Bind each key-function bindings in ARGS using `bind-keys*'.
Nicer wrapper."
  (declare (indent 0))
  `(bind-keys* ,@(mapcar
                  (lambda (kf) `(,(car kf) . ,(intern (symbol-name (cadadr kf)))))
                  (sam--group args 2))))

(sam-defkeys
 "C-/"            'complete-symbol
 "C-S-k"          'kill-whole-line
 "C-x |"          'split-window-right
 "C-x ="          'balance-windows
 "C-x M-c"        'compile
 "M-SPC"          'cycle-spacing
 "M-«"            'beginning-of-buffer
 "M-»"            'end-of-buffer
 "M-s-n"          'forward-paragraph
 "M-s-p"          'backward-paragraph
 "s-c"            'clone-indirect-buffer-other-window
 "s-d"            'kill-buffer-and-window
 "s-u"            'negative-argument
 "H-n"            'make-frame
 "H-u"            'revert-buffer
 "H-<backspace>"  'switch-to-buffer-other-window
 "H-M-p"          'scroll-up-command
 "H-M-n"          'scroll-down-command
 "H-M-s"          'mark-sexp)

(sam-defkeys
  "s-t" 'forward-paragraph
  "s-s" 'backward-paragraph)

(sam-defkeys
  "H-t" 'scroll-down
  "H-s" 'scroll-up)



(provide 'sam-keybindings)
;;; sam-keybindings.el ends here
