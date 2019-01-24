;;; sam-mail.el --- mailing                          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords:

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

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
  :bind* (("<f5>" . mu4e)
          :map mu4e-main-mode-map
          ("n" . next-line)
          ("p" . previous-line)
          :map mu4e-compose-mode-map
          ("M-q" . nil)
          :map mu4e-headers-mode-map
          ("s-c" . mu4e-headers-query-prev)
          ("s-r" . mu4e-headers-query-next))
  :custom
  (mu4e-mu-binary "/usr/local/bin/mu")
  (mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
  (mu4e-maildir "~/Maildir/")
  (mu4e-drafts-folder "/drafts")
  (mu4e-sent-folder "/sent")
  (mu4e-trash-folder "/trash")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-headers-auto-update t)
  (mu4e-use-fancy-chars nil)
  (mu4e-confirm-quit nil)
  (mu4e-compose-format-flowed nil)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-user-mail-address-list '("samuel.barreto8@gmail.com"
                                 "samuel.barreto@univ-lyon1.fr"))
  (mu4e-maildir-shortcuts '(("/gmail/inbox" . ?i)
                            ("/univ/inbox"  . ?u)
                            ("/sent"        . ?s)
                            ("/trash"       . ?t)
                            ("/drafts"      . ?d)))
  (mu4e-bookmarks `(("date:today..now AND NOT flag:trashed" "Today" ,?t)
                    ("flag:unread AND NOT flag:trashed" "Unread" ,?s)
                    ("date:7d..now AND NOT flag:trashed" "Week" ,?r)
                    ("maildir:/univ/inbox" "Univ" ,?n)
                    ("maildir:/gmail/inbox" "Gmail" ,?m)))
  (mu4e-compose-reply-to-address "samuel.barreto8@gmail.com")
  (mu4e-view-show-images t)

  :init
  (add-hook! 'message-mode-hook
    (turn-on-orgtbl))

  :config
  ;; use mu4e as emacs default mail program
  (setq user-mail-address "samuel.barreto8@gmail.com"
        user-full-name "Samuel Barreto")

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; view mail in browser if possible
  (add-to-list
   'mu4e-view-actions
   '("browser" . mu4e-action-view-in-browser) t))

(use-package message
  :after mu4e
  :custom
  (message-send-mail-function 'message-send-mail-with-sendmail)
  ;; tell msmtp to choose the smtp server according to the from field
  (message-send-mail-extra-arguments '("--read-envelope-from"))
  (message-sendmail-f-is-evil 't)
  :hook
  (message-mode . turn-on-orgtbl))

(use-package sendmail
  :after mu4e
  :custom
  (sendmail-program "/usr/local/bin/msmtp"))

(use-package smtpmail
  :after mu4e)

(use-package mwim
  :bind* (("C-a" . mwim-beginning)
          ("C-e" . mwim-end)))

(provide 'sam-mail)
;;; sam-mail.el ends here
