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

(require 'sam-utils)

(use-package dired
  :bind* (("C-x d" . dired-other-window))
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
      (with-current-buffer buffer
        (dired-hide-details-mode t)
        (setq-local right-fringe-width 0)
        (setq-local left-fringe-width 0)
        (setq-local mode-line-format nil)
        (text-scale-set -3))
      (sam-side-buffer
       buffer  `((side          . left)
                 (slot          . 4)
                 (window-width  . 20)
                 (preserve-size . (t . nil)) ,sam--parameters))))



  (let ((gls "/usr/local/bin/gls"))
    (if (file-exists-p gls)
        (setq insert-directory-program gls)))

  (setq ls-lisp-use-insert-directory-program t)
  (setq dired-listing-switches "-alhXg")
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

  (defvar sam-dired--dir-prefix
    (let ((letters (split-string "abcdefghijklmnopqrstuvwxyz" "" t)))
      (apply #'append
             (seq-map (lambda (x) (seq-map (lambda (y) (concat x y)) letters)) letters)))
    "List of potential prefix used for directory naming.")

  (defun sam-dired--make-dir-prefix ()
    "Return a prefix of the form aa, ab, ac, ... for creating
a directory."
    (let* ((dirs (seq-filter #'file-directory-p (directory-files default-directory)))
           (prefs (mapcar (lambda (x) (elt (split-string x "_" t) 0)) dirs))
           (lpref sam-dired--dir-prefix))
      (elt (seq-difference lpref prefs #'equal) 0)))

  (defun sam-project--make-readme (pname)
    "Create a template of README file"
    (with-current-buffer (find-file-noselect "README")
      (org-mode)
      (goto-char 1)
      (insert (format "# %s" "-*- mode: org -*-\n"))
      (insert (format "#+TITLE: %s \n" pname))
      (insert "\n\n* Description\n\n")
      (save-buffer)))

  (defun sam-project--create-dirs (external-data)
    "Create the directory structure."
    (cond
     (external-data
      (let ((inp (read-file-name "Where is input data stored? "))
            (oup (read-file-name "Where is output data stored? ")))
        (make-symbolic-link inp "i")
        (make-symbolic-link oup "o")))
     (t
      (make-directory "i")
      (make-directory "o")))
    (mapc #'make-directory '("etc" "scripts" "analysis" ".bkp" ".log"))
    (when (eq major-mode 'dired-mode)
      (revert-buffer)))

  (defun sam-project--git-init ()
    (with-current-buffer (find-file-noselect ".gitignore")
      (insert
       (mapconcat #'identity '("etc/" ".bkp/" "i/" "i" "o/" "o" ".log/") "\n"))
      (save-buffer))
    (magit-init default-directory)
    (magit-stage-file "README")
    (magit-stage-file ".gitignore"))

  (defun sam-make-project (pname external-data)
    (interactive
     (list (read-string "Project Name: ")
           (yes-or-no-p "Is data external? ")))
    (sam-project--make-readme pname)
    (sam-project--create-dirs external-data)
    (sam-project--git-init))

  (defun dired-mkdir-date (dir-name)
    "Make a directory with current date style"
    (interactive "sDirectory content: ")
    (mkdir (format "%s_%s_%s"
                   (sam-dired--make-dir-prefix)
                   (replace-regexp-in-string " " "-" dir-name)
                   (format-time-string "%Y-%m-%d" (current-time))))
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
   ("ù"   . dired-up-directory)
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
   ("K"   . sam-dired-rm)
   ("C-'" . shell)))

(use-package dired-x
  :after dired
  :bind* (("C-x C-d" . dired-jump))
  :commands (dired-omit-mode)
  :init
  (add-hook! 'dired-load-hook
    (load "dired-x"))
  (add-hook! 'dired-mode-hook
    (dired-omit-mode)
    (dired-hide-details-mode))
  :config
  (setq dired-omit-verbose nil)
  (setq dired-omit-extensions
        (cl-list* ".aux" ".fls" ".log" ".out" ".toc" ".fdb_latexmk" ".bbl" ".blg" ".bcf" ".run.xml" ".x" ".d" dired-omit-extensions))
  (setq dired-omit-files (concat dired-omit-files
                                 "\\|^\\..*$\\|^.DS_Store$\\|^.projectile$\\|^.git$")))

(use-package dired-tar
  :after dired
  :bind* (:map dired-mode-map
          ("T" . dired-tar-dwim)))

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

(defun sam-dired-rm ()
  "Remove marked files using rm."
  (interactive)
  (progn
    (when (y-or-n-p "Remove? ")
      (dired-do-shell-command
       "rm -f * &" nil (dired-get-marked-files))
      (dired-do-redisplay))))

(use-package dired-toggle
  :bind* (("<f3>" . #'dired-toggle)
          ("C-x C-'" . #'dired-toggle))
  :bind (:map dired-mode-map
         ("q" . #'dired-toggle-quit)
         ([remap dired-find-file] . #'dired-toggle-find-file)
         ([remap dired-up-directory] . #'dired-toggle-up-directory)
         ("C-c C-u" . #'dired-toggle-up-directory))
  :config
  (setq dired-toggle-window-size 32)
  (setq dired-toggle-window-side 'left)

  ;; Optional, enable =visual-line-mode= for our narrow dired buffer:
  (add-hook 'dired-toggle-mode-hook
            (lambda () (interactive)
              (visual-line-mode 1)
              (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
              (setq-local word-wrap nil))))

(setq dired-garbage-files-regexp
      (concat (regexp-opt
               '("aux" "bak" "log" "dvi" "orig" "rej" "toc"
                 "bbl" "bcf" "blg" "fdb_latexmk" "fls" "out"
                 "run\.xml"))
	          "\\'"))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'sam-dired)
;;; sam-dired.el ends here
