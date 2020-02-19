;;; sam-completion.el --- completion and helpers     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: completion

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

(require 'ivy)
(require 'ivy-overlay)
(require 'company)

(setq company-tooltip-align-annotations t)
(setq company-idle-delay 0.2
      company-selection-wrap-around t
      company-minimum-prefix-length 3
      company-require-match nil
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil
      company-show-numbers t)

(bind-keys :map company-active-map
  ("C-d" . company-show-doc-buffer)
  ("C-l" . company-show-location)
  ("C-n" . company-select-next)
  ("C-p" . company-select-previous)
  ("C-t" . company-select-next)
  ("C-s" . company-select-previous)
  ("TAB" . company-complete))

(add-hook 'prog-mode-hook 'company-mode)

(defun sam-load-theme ()
  "Helper for `counsel-load-theme' that pops up theme in an
overlay at point."
  (interactive)
  (let ((ivy-display-function 'ivy-display-function-overlay))
    (counsel-load-theme)))

(defalias 'slt 'sam-load-theme)

(provide 'sam-completion)
;;; sam-completion.el ends here
