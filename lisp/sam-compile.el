;;; sam-compile.el --- makefile editing              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: make

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

(use-package make-mode
  :functions (sam-makefile-info
              sam-makefile-document)
  :commands (makefile-bsdmake-mode
             makefile-gmake-mode
             makefile-pickup-targets)
  :init
  (add-hook! 'makefile-bsdmake-mode-hook
    (makefile-gmake-mode))
  :config

  (defun sam-makefile--search-vars (buf)
    (with-current-buffer buf
      (let ((vars '()))
        (mapc
         (lambda (s)
           (save-excursion
             (goto-char 0)
             (while (search-forward s nil t)
               (setq vars
                     (append vars
                             `(,(buffer-substring-no-properties (point-at-bol)
                                                                (- (point) 4))))))))
         '(" := " " ?= "))
        vars)))


  (defun sam-makefile-info ()
    (interactive)
    (when (eq major-mode 'makefile-gmake-mode)
      (let ((vars (sam-makefile--search-vars (current-buffer))))
        (insert ".PHONY: info\ninfo:")
        (cl-loop for var in vars do
                 (insert (format "\n\t@echo %s: $(%s)" var var)))
        (insert "\n"))))

  (defun sam-makefile-document ()
    (interactive)
    (require 'make-mode)
    (progn
      (makefile-pickup-targets)
      (let*
          ((targets makefile-target-table)
           (excluded-targets '(".PHONY" ".PRECIOUS"))
           (targets
            (seq-filter
             (lambda (x) (not (or (cl-member (car x) excluded-targets :test 'string-equal)
                             (string-match "%" (car x)))))
             targets)))
        (insert "\n.PHONY: help\nhelp:\n")
        (mapcar
         (lambda (x) (insert (format "\t$(info make %-10s -- )\n" (car x))))
         targets)))))

(provide 'sam-compile)
;;; sam-compile.el ends here
