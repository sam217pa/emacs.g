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

(use-package dired
  :bind* (("C-x d" . dired-other-window)
          ("C-x C-d" . dired-side))
  :defines (ls-lisp-use-insert-directory-program)
  :commands (dired
             dired-view-other-window
             dired-side
             dired-get-file-for-visit
             dired-noselect
             dired-hide-details-mode
             dired-next-line
             dired-previous-line
             dired-find-file
             dired-up-directory
             dired-get-marked-files)
  :functions (dired-side
              dired-mkdir-date
              dired-mkdir-date-rstyle)
  :config
  (defun dired-side ()
    "Display `default-directory' in side window on left, hiding details."
    (interactive)
    (let ((buffer (dired-noselect default-directory)))
      (with-current-buffer buffer (dired-hide-details-mode t))
      (display-buffer-in-side-window
       buffer  `((side          . left)
                 (slot          . 0)
                 (window-width  . 20)
                 (preserve-size . (t . nil)) ,sam--parameters))))

  (let ((gls "/usr/local/bin/gls"))
    (if (file-exists-p gls)
        (setq insert-directory-program gls)))

  (setq ls-lisp-use-insert-directory-program t)
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t) ; guess copy target based on other dired window

  (defun dired-view-other-window ()
    "View the current file in another window (possibly newly created)."
    (interactive)
    (if (not (window-parent))
        (split-window))
    (let ((file (dired-get-file-for-visit))
          (dbuffer (current-buffer)))
      (other-window 1)
      (unless (equal dbuffer (current-buffer))
        (if (or view-mode (equal major-mode 'dired-mode))
            (kill-buffer)))
      (let ((filebuffer (get-file-buffer file)))
        (if filebuffer
            (switch-to-buffer filebuffer)
          (view-file file))
        (other-window -1))))

  (defun dired-mkdir-date (dir-name)
    "Make a directory with current date style"
    (interactive "sDirectory content: ")
    (mkdir (format "%s-%s" (format-time-string "%Y-%m-%d" (current-time)) dir-name))
    (revert-buffer))

  (defun dired-mkdir-date-rstyle (dir-name)
    (interactive "sDirectory content: ")
    (mkdir (format "%s.%s" dir-name (format-time-string "%Y%m%d" (current-time))))
    (revert-buffer))

  (defun dired-mktemp ()
    (interactive)
    (mkdir (format ".tmp-%s" (format-time-string "%F-%H%M%S")))
    (revert-buffer)
    (dired-omit-mode -1))

  (bind-keys
   :map dired-mode-map
   ("SPC" . dired-view-other-window)
   ("C-+" . dired-mktemp)
   ("H"   . dired-omit-mode)
   ("h"   . dired-omit-mode)
   ("t"   . dired-next-line)
   ("s"   . dired-previous-line)
   ("r"   . dired-find-file)
   ("c"   . dired-up-directory)
   ("8"   . dired-mkdir-date)
   ("9"   . dired-mkdir-date-rstyle)
   ("O"   . sam-open-in-external-app)
   ("C-'" . shell)
   ("q"   . (lambda () (interactive) (quit-window 4)))))

(use-package dired-x
  :after dired
  :bind* (("C-x C-'" . dired-jump))
  :commands (dired-omit-mode)
  :init
  (add-hook! 'dired-load-hook
    (load "dired-x"))
  (add-hook! 'dired-mode-hook
    (dired-omit-mode))
  :config
  (setq dired-omit-verbose nil)
  (setq dired-omit-extensions
        (cl-list* ".aux" ".fls" ".log" ".out" ".toc" ".fdb_latexmk" ".bbl" ".blg" ".bcf" ".run.xml" ".x" ".d" dired-omit-extensions))
  (setq dired-omit-files (concat dired-omit-files
                                 "\\|^\\..*$\\|^.DS_Store$\\|^.projectile$\\|^.git$")))

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
