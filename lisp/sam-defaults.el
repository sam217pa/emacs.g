;;; sam-defaults.el --- Adjust emacs defaults. -*- lexical-binding: t -*-

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Maintainer: Samuel Barreto <samuel.barreto8@gmail.com>
;; Version: 1.0.0
;; Keywords: defaults, customization

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is a list of customization I have done on basic emacs
;; experience, tailored for macOS. It adjust windows parameters,
;; buffer behavior, keyboard bindings, fonts, themes and certain
;; aliases.


;;; Code:
;;;; Custom

(defgroup sam nil
  "Customization for my personnal variables and functions."
  :group 'convenience
  :version 1.0
  :prefix "sam-")

(defcustom sam-font "DejaVu Sans Mono 11"
  "Font for coding situations."
  :group 'sam
  :type 'string)

(defcustom sam-variable-pitch-font "Input Sans Narrow"
  "Font for text"
  :group 'sam
  :type 'string)

(defcustom sam-theme 'leuven
  "Default theme for my config"
  :group 'sam
  :type 'theme)

(defcustom sam-use-variable-pitch-font t
  "Whether to use a variable pitch font for non-coding situations
or not.

Defaults to t."
  :group 'sam
  :type 'boolean)

;;;; Macros

;;;###autoload
(defmacro add-hook! (hook &rest body)
  "Nicer add-hooking that prevents writing lambdas explicitely.

Add a lamdba containing BODY to hook HOOK."
  (declare (indent 1))
  `(add-hook ,hook
             (lambda () (progn ,@body))))

;;;; Default adjustment

(defun sam--keyboard! ()
  "Defines the default keyboard behavior on darwin."
  (when (eq system-type 'darwin)
    (setq
     ;; cmd de droite = meta
     mac-right-command-modifier 'meta
     ;; cmd de gauche = control
     mac-command-modifier 'control
     ;; option de gauche = super
     mac-option-modifier 'super
     ;; option de droite = carac spéciaux
     mac-right-option-modifier nil
     ;; control de gauche = hyper (so does capslock)
     mac-control-modifier 'hyper
     mac-right-control-modifier 'alt
     ;; fn key = hyper
     ns-function-modifier 'alt
     ;; cette touche n'existe p
     ns-right-alternate-modifier nil)))

(defun sam--darwin-defaults! ()
  "Set the default behavior with respect to a macosx environment."
  ;; disable system call to commands like C-h (hide frame on macOS by default)
  (setq mac-pass-command-to-system nil)
  ;; idem
  (setq mac-pass-control-to-system nil)
  (setq-default locate-command "mdfind")
  (setq-default cursor-type 'box)

  (setq delete-by-moving-to-trash t))

(defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash") nil nil nil file))

(defun sam--environment! ()
  (setenv "INFOPATH" (expand-file-name "~/.local/share/info:"))
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8"))

(defun sam--defaults! ()
  (add-to-list 'fill-nobreak-predicate 'fill-french-nobreak-p)
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 10)
  (setq
   tramp-password-prompt-regexp
   (concat
    "^.*"
    (regexp-opt
     '("passphrase" "Passphrase"
       ;; English
       "password" "Password"
       ;; Deutsch
       "passwort" "Passwort"
       ;; Français
       "mot de passe" "Mot de passe") t)
    ".*:\0? *"))

  (add-hook! 'Info-mode-hook
    (add-to-list 'Info-directory-list
                 (expand-file-name "~/.local/share/info")))

  (setq
   ;; use-package décrit les appels qu'il fait
   use-package-verbose nil
   ;; supprime les vieilles versions des fichiers sauvegardés
   delete-old-versions -1
   ;; enable le version control
   version-control t
   ;; backups file even when under vc
   vc-make-backup-files t
   ;; vc suit les liens  symboliques
   vc-follow-symlinks t
   ;; supprime l'écran d'accueil
   inhibit-startup-screen t
   ;; supprime cette putain de cloche.
   ring-bell-function 'ignore
   ;; sentences does not end with double space.
   sentence-end-double-space nil
   default-fill-column 72
   initial-scratch-message ""
   save-interprogram-paste-before-kill t
   ;; focus help window when opened
   help-window-select t
   ;; tab are 4 spaces large
   tab-width 4
   auto-window-vscroll nil)

  ;; finer grain of text increasing
  (setq text-scale-mode-step 1.05)

  ;; transforme les noms des fichiers sauvegardés
   (setq auto-save-file-name-transforms
         `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; store all backup and autosave files in the tmp dir
  (setq backup-directory-alist
	`((".*" . ,(no-littering-expand-var-file-name "backups/"))))

  ;; store custom in etc
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

  (setq-default indent-tabs-mode nil
		        tab-width 4)

  (setq epg-gpg-program "gpg2")

  ;; utf-8 est le systeme par défaut.
  (prefer-coding-system 'utf-8)

  ;; remplace yes no par y n
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'kill-frame #'delete-frame)
  (defalias 'kill-other-frames #'delete-other-frames)
  (defalias 'kill-window #'delete-window)
  (defalias 'kill-other-windows #'delete-other-windows)

  ;; display line number in mode line
  (line-number-mode -1)
  ;; display colum number in mode line
  (column-number-mode -1)
  (save-place-mode)
  ;; replace highlighted text with type
  (delete-selection-mode 1)

  (setq initial-major-mode 'fundamental-mode)

  ;; supprime les caractères en trop en sauvegardant.
  (add-hook! 'before-save-hook
    (delete-trailing-whitespace))

  ;; rend les scripts executable par défault si c'est un script.
  (add-hook! 'after-save-hook
    (executable-make-buffer-file-executable-if-script-p)))

(defun sam--appearances! ()
  "Set the default theme and fonts"
  (unless (eq sam-theme 'default)
    (load-theme sam-theme t))
  (set-face-attribute
   'variable-pitch
   nil
   :family sam-variable-pitch-font
   :height 140)

  (when sam-use-variable-pitch-font
    (remove-hook 'text-mode-hook
      (variable-pitch-mode 1)))

  (when window-system
    ;; increase space between lines
    (setq-default line-spacing 0)

    ;; change default font for current frame
    (add-to-list 'default-frame-alist `(font . ,sam-font))
    (add-to-list 'default-frame-alist `(:height . 150))
    (set-face-attribute 'default nil :font sam-font :height 150)))

(defun sam--initialize-frame! ()
  "Set the default dimension and position of a new frame."
  (let* ((a-width (* (display-pixel-width) 0.50))
         (a-height (* (display-pixel-height) 0.60))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))

    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame)
                    (truncate a-width)
                    (truncate a-height)
                    t )))


(defun sam--windows! ()
  "Set the default behavior for windows and splitting buffers."
  (defvar sam--parameters
    '(window-parameters . ((no-other-window . t)
                           (no-delete-other-windows . t))))

  (setq fit-window-to-buffer-horizontally t)
  (setq window-resize-pixelwise t)
  (setq frame-resize-pixelwise t)

  (setq display-buffer-alist
        `(("\\*\\(?:Buffer List\\|Bookmark List\\|Bookmark Annotation\\)\\*"
           display-buffer-in-side-window
           (side . top)
           (slot . -1)
           (window-height . fit-window-to-buffer)
           (preserve-size . (nil . t)) ,sam--parameters)
          ("\\*Buffer List\\*" display-buffer-in-side-window
           (side . top)
           (slot . -1)
           (window-height . 10)
           (preserve-size . (nil . t)) ,sam--parameters)
          ("\\*ibuffer\\*" display-buffer-in-side-window
           (side . top)
           (slot . -1)
           (window-height . 10)
           (preserve-size . (nil . t)) ,sam--parameters)
          ("\\*Tags List\\*" display-buffer-in-side-window
           (side . right)
           (slot . 1)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil)) ,sam--parameters)
          ("\\*toc\\*" display-buffer-in-side-window
           (side . left)
           (slot . 2)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil)) ,sam--parameters)
          ("\\*\\(?:help\\|grep\\|Completions\\|undo-tree\\)\\*"
           display-buffer-in-side-window
           (side . left)
           (window-width . fit-window-to-buffer)
           (slot . -1)
           (preserve-size . (t . nil))
           ,sam--parameters)
          ("\\*\\(?:shell\\|Async Shell Command\\)\\*" display-buffer-in-side-window
           (side . top)
           (slot . 1)
           (preserve-size . (nil . t))
           ,sam--parameters)
          ("\\*\\(?:compilation\\|interpretation\\)\\*" display-buffer-in-side-window
           (side . bottom)
           (slot . -1)
           (preserve-size . (nil . t))
           ,sam--parameters)
          ("\\*Org Select\\*" display-buffer-in-side-window
           (side . top)
           (slot . -1)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil))
           ,sam--parameters)
          ("\\*Agenda Commands\\*" display-buffer-in-side-window
           (side . left)
           (slot . -1)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil))
           ,sam--parameters)
          ("\\*ivy\\*" display-buffer-in-side-window
           (side . left)
           (slot . 1)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil))
           ,sam--parameters))))

;;;###autoload
(defun sam-initialize! ()
  (interactive)
  (sam--initialize-frame!)
  (sam--windows!)
  (sam--appearances!)
  (sam--environment!)
  (sam--defaults!)
  (sam--keyboard!)
  (sam--darwin-defaults!))

(add-hook 'before-save-hook #'time-stamp)

(defmacro advices-add (symbol &rest args)
  "Batch advice SYMBOL with ARGS."
  (declare (indent 1))
  `(progn
     ,@(mapcar
        (lambda (arg) `(advice-add ,symbol ,@arg))
        args)))

(defun advice-backward-word (&rest funs)
  "Advice FUNS with `backward-word'."
  (cl-loop
   for fun in funs
   do (advices-add fun
        (:before #'sam--bwd-advice)
        (:after  #'sam--fwd-advice))))

(defun sam--bwd-advice (&optional arg)
  (cond
   ((looking-at "[[:blank:]]")
    (backward-word arg))))

(defun sam--fwd-advice (&optional arg)
  (forward-char))

(advice-backward-word #'capitalize-word #'upcase-word #'downcase-word)

(add-hook 'text-mode-hook #'electric-quote-local-mode)

(add-to-list 'load-path "~/dotfile/emacs/private/cf/")
(add-to-list 'load-path "~/dotfile/emacs/private/eaf/")

(provide 'sam-defaults)

;;; sam-defaults.el ends here
