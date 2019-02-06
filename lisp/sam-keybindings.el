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

;; This file gathers keybindings of mine. The goal is to work towards
;; a more granular approach than a single big init file that can't be
;; compiled without taking ages.

;;; Code:

(require 'bind-key)
(require 'sam-utils)

;;;###autoload
(defmacro sam-defkeys (&rest args)
  "Bind each key-function bindings in ARGS using `bind-keys*'.
Nicer wrapper."
  (declare (indent 0))
  `(bind-keys*
    ,@(mapcar
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


(sam-defkeys
  "H-'"     'sam-iterm-here
  "H-l"     'sam-duplicate-line
  "H-o"     'sam-reveal-in-finder
  "H-w"     'sam-maximize-window
  "s-<tab>" 'sam-switch-to-other-buffer
  "s-I"     'sam-indent-paragraph
  "s-j"     'sam-join-to-next-line
  "s-n"     'sam-narrow-or-widen-dwim
  "s-o"     'sam-open-in-external-app
  "s-q"     'sam-unfill-paragraph
  "s-w"     'sam-main-window
  "ð"       'sam-kill-word-at-point
  "s-C"     'sam-switch-to-compilation
  "C-x n"   'sam-narrow-or-widen-dwim)

(use-package key-chord
  :commands (key-chord-mode
             key-chord-define-global)
  :custom
  (key-chord-two-key-delay 0.2)
  :init
  (key-chord-mode 1)
  (key-chord-define-global "xq" #'fort))


(use-package key-seq
  :after key-chord
  :commands (key-seq-define-global
             key-seq-define)
  :init
  (key-seq-define-global "qd" #'bury-buffer)
  (key-seq-define-global "qb" #'counsel-bookmark)
  (key-seq-define-global "qf" #'kill-frame)
  (key-seq-define-global "qw" #'kill-window)
  (key-seq-define emacs-lisp-mode-map "$u" #'use-package-jump))

(provide 'sam-keybindings)
;;; sam-keybindings.el ends here
