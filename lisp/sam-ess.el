;;; sam-ess.el --- helpers for ess                   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: ess, stats, Rstat

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

(with-eval-after-load 'ess-site
  (require 'ess-r-mode)
  (setq ess-eval-visibly nil)
  (setq ess-offset-continued 2)
  (setq ess-expression-offset 2)
  (setq ess-nuke-trailing-whitespace-p t)

  (setq ess-help-reuse-window t)
  (setq ess-use-ido nil)
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:constants . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:%op% . t)))

  (ess-set-style 'DEFAULT)

  (bind-keys
   :map ess-r-mode-map
   ("RET" . newline-and-indent)
   ("S-<return>" . ess-eval-line)
   ("C-RET" . ess-eval-line)
   ("C-<return>" . ess-eval-region-or-function-or-paragraph)
   ("M-RET" . ess-eval-function-or-paragraph)
   ("C-c M-s" . ess-switch-process)
   ("C-c C-p" . ess-eval-paragraph)
   ("_" . self-insert-command)
   (" " . ess-insert-assign)
   (" " . ess-insert-assign))

  (add-hook! 'ess-r-mode-hook
    (setq-local outline-regexp "^# \\*"))
  (add-to-list 'hs-special-modes-alist
               '(ess-r-mode "{" "}" "#[#']" nil nil))
  (sp-local-pair
   'ess-mode "{" nil
   :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair
   'ess-mode "(" nil
   :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'ess-mode "%" "%")
  (sp-local-pair 'ess-mode "`" "`" :actions '(wrap insert) :when '(sp-in-comment-p))

  (require 'ess-inf)
  ;; (bind-keys
  ;;  :map inferior-ess-mode-map
  ;;   ("_" . self-insert-command))
  (add-hook 'inferior-ess-mode-hook (lambda () (text-scale-set -5)))

  (use-package lesspy
    :hook (ess-mode . lesspy-mode)
    :bind (:map lesspy-mode-map
           ("a" . lesspy-avy-jump)
           ("p" . lesspy-eval-function-or-paragraph)
           ("h" . lesspy-help)
           ("l" . lesspy-eval-line)
           ("L" . lesspy-eval-line-and-go)
           ("e" . lesspy-eval-sexp)
           ("E" . lesspy-avy-eval)
           ("c" . lesspy-left)
           ("t" . lesspy-down)
           ("s" . lesspy-up)
           ("r" . lesspy-right)
           ("d" . lesspy-different)
           ("m" . lesspy-mark)
           ("x" . lesspy-execute)
           ("u" . lesspy-undo)
           ("z" . lesspy-to-shell)
           ("(" . lesspy-paren)
           ("»" . lesspy-forward-slurp)
           ("«" . lesspy-backward-slurp)
           ("#" . lesspy-comment)
           ("'" . lesspy-roxigen)
           ("C" . lesspy-cleanup-pipeline)
           ("C-(" . lesspy-paren-wrap-next)))

  (use-package ess-spinr
    :after ess-site))

(setq julia-arguments '("-i" "--color=yes" "-p 4"))
(setq inferior-julia-args
      (mapconcat #'identity julia-arguments " "))

(provide 'sam-ess)
;;; sam-ess.el ends here
