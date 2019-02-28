;;; sam-themes.el --- themes                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: themes, appearances

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

(defmacro sam-use-theme (theme &optional path)
  "Use THEME as a package and add its PATH to `custom-theme-load-path'.

If PATH is nil, this macro will attempt to build its path in .emacs.d/lib/."
  (declare (indent 1))
  (let ((g!path (or path
                    (concat (expand-file-name "~/.emacs.d/lib/")
                            (symbol-name theme)))))
    `(progn
       (use-package ,theme
         :no-require t
         :defer t)
       (add-to-list 'custom-theme-load-path ,g!path))))

(defmacro sam-use-themes (&rest themes)
  "Use all THEMES in themes by adding their path to `custom-theme-load-path'

This macro should be the only entry point to adding a theme to my
emacs.

See also `sam-use-theme'."
  (declare (indent 0))
  `(progn
     (setq custom-theme-load-path nil)  ;; disable all default themes
     ,@(mapcar
        (lambda (x) `(sam-use-theme ,@x))
        (sam--group themes 2))))

(sam-use-themes
  solarized "~/.emacs.d/lib/solarized-theme/"
  zenburn-theme nil)


(use-package minions
  :hook (after-init . minions-mode))

(provide 'sam-themes)
;;; sam-themes.el ends here
