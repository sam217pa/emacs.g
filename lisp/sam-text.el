;;; sam-text.el --- text deriving editing            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: text

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

;; Code for editing text related stuff, like markdown.

;;; Code:


(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :hook ((markdown-mode . outline-minor-mode)
         (markdown-mode . abbrev-mode)
         (markdown-mode . shelter-mode)))


(provide 'sam-text)
;;; sam-text.el ends here
