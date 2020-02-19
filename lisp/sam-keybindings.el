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
;; (require 'shelter)

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
  "C-c M" 'sam-find-makefile
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
  "H-M-s"          'mark-sexp
  ;; "s-." 'sam-kill-ring-save-dwim
  )

(sam-defkeys
  "s--" #'sam-rule-to-eol
  "s-t" 'forward-paragraph
  "s-s" 'backward-paragraph
  "s-m" 'next-match
  "s-M" 'previous-match)

(sam-defkeys
  "H-t" 'forward-page
  "H-s" 'backward-page)


(sam-defkeys
  "H-'"     #'sam-iterm-here
  "H-l"     #'sam-duplicate-line
  "H-o"     #'sam-reveal-in-finder
  "H-w"     #'sam-maximize-window
  "s-<tab>" #'sam-switch-to-other-buffer
  "s-I"     #'sam-indent-paragraph
  "s-j"     #'sam-join-to-next-line
  "s-n"     #'sam-narrow-or-widen-dwim
  "s-o"     #'sam-open-in-external-app
  "s-f"     #'project-find-file
  "s-q"     #'sam-defill-paragraph
  "s-w"     #'sam-main-window
  "ð"       #'sam-kill-word-at-point
  "s-C"     #'sam-switch-to-compilation
  "C-x n"   #'sam-narrow-or-widen-dwim
  "C-c n l" #'sam-note
  "C-x -"   #'split-window-below)

(use-package key-chord
  :commands (key-chord-mode
             key-chord-define-global)
  :custom
  (key-chord-two-key-delay 0.2)
  :init
  (key-chord-mode 1))

(defun sam-hrz-project ()
  "Find an horizon project"
  (interactive)
  (counsel-find-file "~/hrz/p/"))

(use-package key-seq
  :after key-chord
  :commands (key-seq-define-global
             key-seq-define)
  :init
  (key-seq-define-global "QD" #'kill-this-buffer)
  (key-seq-define-global "qd" #'bury-buffer)
  (key-seq-define-global "qb" #'counsel-bookmark)
  (key-seq-define-global "qh" #'sam-hrz-project)
  (key-seq-define-global "qf" #'kill-frame)
  (key-seq-define-global "qw" #'kill-window)
  (key-seq-define emacs-lisp-mode-map "$u" #'use-package-jump))


(use-package which-key
  :defer 2
  :diminish which-key-mode
  :commands (which-key-mode
             which-key-setup-side-window-right-bottom
             which-key-add-key-based-replacements)
  :custom
  ;; simple then alphabetic order.
  (which-key-sort-order 'which-key-key-order)
  (which-key-popup-type 'side-window)
  (which-key-side-window-max-height 0.3)
  (which-key-side-window-max-width 0.5)
  (which-key-idle-delay 0.3)
  (which-key-min-display-lines 7)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  ;; key description for C-x
  (which-key-add-key-based-replacements
    "C-x RET" "coding system -input"
    "C-x 4"   "Other Window"
    "C-x 5"   "Frame"
    "C-x 6"   "2C"
    "C-x @"   "event"
    "C-x 8"   "special char"
    "C-x a"   "abbrev"
    "C-x n"   "narrow"
    "C-x r"   "rectangle"
    "C-x v"   "version control"
    "C-c &"   "yas"
    "C-c @"   "hide-show"
    "M-SPC h" "info"
    "M-SPC g" "grep"
    "M-SPC M-s" "occur"))

;; (fort-define-keys
;;  :map fort-keymap
;;  :simple
;;   ("t" 'next-line
;;    "s" 'previous-line
;;    "r" 'forward-char
;;    "c" 'backward-char
;;    "SPC b" 'ivy-switch-buffer
;;    "SPC <tab>" 'sam-switch-to-other-buffer))

(global-set-key (kbd "s-P") nil)        ;remove printer command
(global-set-key (kbd "s-p") nil)        ;nobody wants to print an
                                        ;emacs buffer.
(global-set-key (kbd "s-m") nil)


(provide 'sam-keybindings)
;;; sam-keybindings.el ends here
