;;; sam-prog.el --- programmation                    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: prog, abbrev

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

(use-package mwim
  :bind* (("C-a" . mwim-beginning)
          ("C-e" . mwim-end)))

(use-package leap
  :commands (leap-minor-mode
             leap-hide-all
             leap-show-all
             leap-ref
             leap-lab
             leap-buffer))

(use-package indent-guide
  :commands (indent-guide-mode)
  :custom
  (indent-guide-char "·"))

(use-package nim-mode
  :mode ("\\.nim\\'" . nim-mode)
  :hook (nim-mode-hook . indent-guide-mode)
  :init
  (add-hook! 'nim-mode-hook
    (auto-fill-mode 0)))

(use-package nim-suggest
  :hook (nim-mode-hook . nimsuggest-mode)
  :custom
  (nimsuggest-path "/usr/local/bin/nimsuggest")
  :init
  (add-hook 'nimsuggest-mode-hook 'company-mode)
  (add-hook 'nimsuggest-mode-hook 'flymake-mode))

(use-package cperl-mode
  :mode (("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
  :init
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
  (add-hook 'cperl-mode-hook 'sam-cperl-mode-hook t)

  (defun sam-cperl-sub-doc ()
    ""
    (interactive)
    (let ((subname
           (save-excursion
             (search-forward "sub")
             (let* ((p (point))
                    (sub (and (forward-symbol 1)
                              (buffer-substring-no-properties p (point)))))
               sub))))
      (insert (format "=head3%s



=cut" subname))
      (previous-line 2)))

  (defun sam-cperl-mode-hook ()
    (setq cperl-indent-level 4)
    (setq cperl-continued-statement-offset 0)
    (setq cperl-extra-newline-before-brace t)
    (set-face-background 'cperl-hash-face  nil)
    (set-face-background 'cperl-array-face nil)
    (set-face-underline  'cperl-hash-face nil)
    (set-face-foreground 'cperl-array-face "#6c71c4")
    (set-face-foreground 'cperl-hash-face  "#d33682"))

  (defun sam-perltidy ()
    (interactive)
    (let* ((f (buffer-file-name))
           (ftdy (concat f ".tdy") ))
      (shell-command (format "perltidy %s" (shell-quote-argument f)))
      (ediff f ftdy )
      (delete-file ftdy))))

;; Use cperl-mode instead of the default perl-mode

(use-package prolog
  :mode (("\\.pl\\'" . prolog-mode)
         ("\\.m\\'" . mercury-mode))
  :custom
  (prolog-system 'gnu)
  (prolog-program-name "/Users/samuelbarreto/.local/xsb-3.8.0/bin/xsb")
  (prolog-program-switches
   '((swi ("-G128M" "-T128M" "-L128M" "-O"))
     (t nil)))
  (prolog-electric-if-then-else-flag t))

(use-package ediprolog
  :after prolog
  :bind (:map prolog-mode-map
         ("C-c C-e" . #'ediprolog-dwim)))

(provide 'sam-prog)
;;; sam-prog.el ends here
