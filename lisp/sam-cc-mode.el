;;; sam-cc-mode.el --- c programming                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: c, prog

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

;; This is my customisation for editing c code. Fortunately, emacs is
;; very well suited to programming C, so I don't have to customise
;; that much. I just added ggtags, browsing with GNU Globals is very
;; much to my linking. I would be glad to be able to browse distant
;; sources, like the libguile.h headers, but I still can't tailor
;; ggtags to do that. Maybe with another LSP backend, like eglot.

;;; Code:

(use-package ggtags
  :hook ((cc-mode . ggtags-mode)))

(provide 'sam-cc-mode)
;;; sam-cc-mode.el ends here
