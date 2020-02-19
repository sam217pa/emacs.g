;;; samuelbarreto.el --- personal configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: config

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



;;;; Defaults

(use-package no-littering
  :commands (no-littering-expand-var-file-name
             no-littering-expand-etc-file-name)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


(use-package sam-defaults
  :load-path "~/.emacs.d/lisp/"
  :custom
  (sam-theme 'spacemacs-dark)
  :hook
  (after-init . sam-initialize!))

;;;; Packages

(use-package abbrev
  :hook (text-mode-hook . abbrev-mode)
  :commands (define-abbrev)
  :config
  (setq abbrev-file-name (no-littering-expand-etc-file-name "abbrev_defs"))
  (setq save-abbrevs 'silently)
  (setq only-global-abbrevs nil))


(use-package ace-window
  :bind* (("M-é" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?t ?s ?r ?n ?m ?a ?u ?i ?e))
  (aw-background t)
  (aw-ignore-current t))

(use-package ansi-color
  :commands (ansi-color-apply-on-region)
  :config
  (defun display-ansi-colors ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (ignore-errors
    (defun my-colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)))

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
         ("s-i" . bicycle-cycle)
         ([S-tab] . bicycle-cycle-global)))


(use-package comment-dwim-2
  :bind* ("M-;" . comment-dwim-2))

(use-package conf-mode
  :mode (("DESCRIPTION" . conf-mode)
         ("\\.log\\'" . conf-mode)
         ("\\.toml\\'" . conf-toml-mode)))

(use-package counsel
  :commands (counsel-load-theme
             counsel-bookmark
             counsel-yank-pop)
  :bind* (("C-c i" . counsel-imenu)
          ("C-x C-f" . counsel-find-file)
          ("C-c C-/" . counsel-rg)
          ("M-x" . counsel-M-x))
  :config
  (setq counsel-find-file-ignore-regexp
        dired-garbage-files-regexp)

  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  (setq counsel-find-file-at-point t)

  (defun counsel-font-headings ()
    "Change font of variable pitch."
    (interactive)
    (ivy-read
     "Chose font :"
     (font-family-list)
     :caller 'counsel-font-headings
     :action
      (lambda (x)
        (set-face-attribute 'variable-pitch nil
                            :family x :height 120))))

  (defun counsel-font ()
    "Change font of current frame"
    (interactive)
    (ivy-read "Chose font :"
              (font-family-list)
              :caller 'counsel-font
              :action (lambda (x) (set-frame-font x)))))


(use-package cwl-mode
  :mode ("\\.cwl\\'" . cwl-mode))

(use-package deadgrep
  :bind* (("C-c /" . deadgrep))
  :bind (:map deadgrep-mode-map
         ("s" . deadgrep)))


(use-package debian-control-mode
  :load-path "/Users/samuelbarreto/dotfile/emacs/private/dcf/"
  :mode (("DESCRIPTION" . debian-control-mode)))


(use-package elisp-mode
  :functions (use-package-jump)
  :bind (:map emacs-lisp-mode-map
         ("A-j" . use-package-jump))
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local lisp-indent-function #'sam-lisp-indent-function)))
  :config
  (defun use-package-jump--list-calls ()
    (let ((packages))
      (save-excursion
        (goto-char (point-max))
        (while (beginning-of-defun)
          (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
            (when (string-match "^(use-package \\([^[:space:]\n]+\\)" line)
              (push (cons (match-string-no-properties 1 line) (point))
                    packages)))))
      packages))


  (defun use-package-jump ()
    "Jump to an outer-level `use-package' definition in current buffer."
    (interactive)
    (let ((packages (use-package-jump--list-calls)))
      (goto-char (cdr (assoc (ivy-completing-read "Package: " packages)
                             packages))))))


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :defer 1
  :commands (exec-path-from-shell-initialize
             exec-path-from-shell-copy-env)
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))


(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-°" . er/contract-region))
  :custom
  (expand-region-fast-keys-enabled t))


(use-package flyspell
  :bind* (("C-c M-f" . flyspell-buffer)
          :map flyspell-mode-map
          ("C-c M-n" . flyspell-goto-next-error)))


(use-package ggo-mode
  :mode ("\\.ggo\\'" . ggo-mode))


(use-package grab-mac-link
  :commands (grab-mac-link
             grab-skim-link-to-page
             grab-mac-link-skim-1)
  :functions (grab-skim-link-to-page)
  :config
  (defun grab-skim-link-to-page ()
    (let* ((link (car (grab-mac-link-skim-1)))
           (page-num (cadr (split-string link "::")))
           (page (concat "p. " page-num)))
      (org-make-link-string link page))))


(use-package hideshow
  :bind* (("s-h" . hs-toggle-hiding)))


(use-package hippie-exp
  :bind* (("M-/" . hippie-expand))
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol)))


(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (TeX-mode  . hl-todo-mode))
  :commands (hl-todo-previous
             hl-todo-next
             hl-todo-occur))


(use-package hungry-delete
  :commands (global-hungry-delete-mode
             hungry-delete-mode)
  :init
  (global-hungry-delete-mode))

(use-package hydra
  :demand t
  :commands (defhydra
              hydra-default-pre
              hydra-keyboard-quit
              hydra--call-interactively-remap-maybe
              hydra-show-hint
              hydra-set-transient-map)
  :functions (hydra-yank-pop/yank
              hydra-yank-pop/yank-pop)
  :bind* (("C-y" . hydra-yank-pop/yank)
          ("M-y" . hydra-yank-pop/yank-pop))
  :config
  (defhydra hydra-yank-pop (:hint nil)
    "yank"
    ("C-y" yank nil)
    ("M-y" yank-pop nil)
    ("y" (yank-pop 1) "next")
    ("p" (yank-pop 1) "next")
    ("n" (yank-pop -1) "prev")
    ("k" counsel-yank-pop "list" :color blue)))


(use-package ibuffer
  :bind* (("C-x C-b" . ibuffer))
  :defines (ibuffer-show-empty-filter-groups)
  :commands (ibuffer
             ibuffer-switch-to-saved-filter-groups
             ibuffer-visit-buffer
             ibuffer-backward-filter-group
             ibuffer-forward-filter-group)
  :config
  (add-hook! 'ibuffer-hook
    (ibuffer-switch-to-saved-filter-groups "Default"))

  (bind-keys
   :map ibuffer-mode-map
   ("t" . next-line)
   ("s" . previous-line)
   ("r" . ibuffer-visit-buffer)
   ("c" . ibuffer-backward-filter-group)
   ("p" . ibuffer-backward-filter-group)
   ("n" . ibuffer-forward-filter-group))

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

  (setq-default ibuffer-saved-filter-groups
                `(("Default"
                   ("Perl" (or (mode . cperl-mode)))
                   ("LaTeX" (or (mode . latex-mode)
                                (mode . TeX-mode)
                                (mode . LaTeX-mode)
                                (mode . TeX-latex-mode)))
                   ("Python" (or (mode . python-mode)
                                 (mode . inferior-python-mode)))
                   ("Julia" (or (mode . julia-mode)
                                (mode . ess-julia-mode)
                                (mode . inferior-julia-mode)
                                (mode . inferior-ess-julia-mode)))
                   ("RStat" (or (mode . ess-r-mode)
                                (mode . inferior-ess-mode)
                                (mode . inferior-ess-r-mode)
                                (mode . Rd-mode)))
                   ("C / C++" (mode . c-mode))
                   ("Org" (mode . org-mode))
                   ("Markdown" (mode . markdown-mode))
                   ("Bash" (or (mode . shell-script-mode)))
                   ("Make" (mode . makefile-mode))
                   ("Dired" (mode . dired-mode))
                   ("PDF" (mode . pdf-view-mode))
                   ("Mail" (or (mode . message-mode)
                               (mode . bbdb-mode)
                               (mode . mail-mode)
                               (mode . mu4e-compose-mode)))
                   ("Lisp" (or (mode . emacs-lisp-mode)
                               (mode . common-lisp-mode)
                               (mode . lisp-mode)
                               (name . "\\*inferior-lisp\\*")))
                   ("Scheme" (mode . scheme-mode))
                   ("shell" (or (mode . eshell-mode)
                                (mode . shell-mode)))
                   ("Magit" (or (mode . magit-mode)))
                   ("Temp" (name . "\*.*\*")))))
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-display-summary t))


(use-package iedit
  :bind* (("C-*" . iedit-mode)))


(use-package isearch
  :bind* (("C-s" . isearch-forward))
  :functions (symbol-name-at-point
              current-thing
              isearch-thing)
  :commands (isearch-yank-string)
  :config
  (defun symbol-name-at-point ()
    (let ((symbol (symbol-at-point)))
      (if symbol (symbol-name symbol) "")))

  (defun current-thing ()
    "Return the current \"thing\":
- if the region is active, return the region's text and deactivate the mark
- else return the symbol at point or the empty string."
    (let ((thing (if (region-active-p)
                     (buffer-substring (region-beginning) (region-end))
                   (symbol-name-at-point))))
      (deactivate-mark)
      thing))

  (defun isearch-thing ()
    "Search for the current \"thing\":
- if the region is active, return the region's text and deactivate the mark
- else return the symbol at point or the empty string."
    (interactive)
    (isearch-yank-string (current-thing)))

  (define-key isearch-mode-map (kbd "C-t") #'isearch-thing))


(use-package ispell
  :functions (ispell-word-then-abbrev)
  :commands (ispell-word-then-abbrev
             ispell
             ispell-get-word
             ispell-word)
  :bind* (("s-:" . ispell)
          ("M-i" . ispell-word-then-abbrev))
  :custom
  (ispell-dictionary "francais")
  :config
  (defun ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (car-safe (save-excursion (ispell-get-word nil))))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil
                     (not (bobp)))
                 (not (bobp)))
          (backward-word)
          (backward-char))
        (setq aft (car-safe (save-excursion (ispell-get-word nil)))))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point")))))

(use-package ivy
  :diminish ""
  :commands (ivy-mode)
  :bind* (("s-t" . ivy-switch-buffer)
          ("s-<backspace>" . ivy-switch-buffer)
          :map ivy-mode-map
          ("C-'" . ivy-avy))
  :custom
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode))


(use-package ivy-bibtex
  :bind ("C-x M-b" . ivy-bibtex)
  :config

  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

  (setq bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib"))
  (setq bibtex-completion-library-path '("~/zotero_bib/"))
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-path "~/dotfile/bibliographie/notes.org")

  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))

  (setq bibtex-completion-notes-template-one-file "
* ${author} (${year}): ${title}
:PROPERTIES:
:Custom_ID: ${=key=}
:END:
"))


(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-list
             lorem-ipsum-insert-sentences
             lorem-ipsum-insert-paragraphs))


(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :custom (lua-default-application "luajit"))


(use-package magit-gitflow
  :after magit
  :bind (:map magit-mode-map
         ("%" . magit-gitflow-popup))
  :commands (turn-on-magit-gitflow)
  :config
  (add-hook! 'magit-mode-hook
    (turn-on-magit-gitflow)))


(use-package magit-todos
  :demand t
  :commands (magit-todos-mode)
  :after magit
  :config
  (magit-todos-mode))


(use-package transient
  :functions (define-transient-command))

(use-package outline
   :custom
   (outline-minor-mode-prefix (kbd "C-."))
   :functions (outline-narrow-to-subtree)
   :commands (outline-transient
              outline-minor-mode
              outline-back-to-heading
              outline-end-of-subtree)
   :bind* (("s-." . outline-transient))
   :config
   (defun outline-narrow-to-subtree ()
     "Narrow to region containing the current outline subtree."
     (interactive)
     (narrow-to-region
	  (progn (outline-back-to-heading t) (point))
	  (progn (outline-end-of-subtree)
	         (when (and (looking-at outline-regexp) (not (eobp))) (backward-char 1))
	         (point))))

   (define-transient-command outline-transient ()
     "Navigate a document using transient"
     :transient-suffix 'transient--do-stay
     :transient-non-suffix 'transient--do-warn
     ["Navigate"
      :class transient-columns
      ["Visible"
       ("p" "prev visible heading" outline-previous-visible-heading)
       ("n" "next visible heading" outline-next-visible-heading)]
      ["Same level"
       ("f" "forward" outline-forward-same-level)
       ("b" "backward" outline-backward-same-level)]
      ["Up/down"
       ("u" "up" outline-up-heading)]]
     ["Hide / show"
      :class transient-columns
      ["Hide"
       ("h a" "all" outline-hide-body)
       ("h e" "entry" outline-hide-entry)
       ("h s" "subtree" outline-hide-subtree)
       ("h b" "branches" outline-hide-leaves)
       ("h o" "other" outline-hide-other)]
      ["Show"
       ("s a" "all" outline-show-all)
       ("s e" "entry" outline-show-entry)
       ("s s" "subtree" outline-show-subtree)
       ("s b" "branches" outline-show-branches)
       ("s c" "children" outline-show-children)]]
     ["Edit"
      :class transient-columns
      ["Subtree"
       ("m u" "promote" outline-promote)
       ("m d" "demote" outline-demote)
       ("m p" "move up" outline-move-subtree-up)
       ("m n" "move down" outline-move-subtree-down)]
      ["Entries"
       ("e i" "insert head" outline-insert-heading)
       ("e n" "narrow subtree" outline-narrow-to-subtree)
       ("e m" "mark subtree" outline-mark-subtree)
       ("e k" "kill headers" outline-headers-as-kill)]])

   (define-key
     outline-minor-mode-map
     (kbd "TAB")
     '(menu-item "" nil
                 :filter (lambda (&optional _)
                           (when (outline-on-heading-p)
                             'bicycle-cycle)))))


(use-package paren
  :hook (after-init . show-paren-mode))


(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))


(use-package rainbow-mode
  :commands (rainbow-mode))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package selected
  :demand t
  :commands (selected-global-mode)
  :functions (org--wrap
              org-enwrap!
              selected-search)
  :bind (:map selected-keymap
         ("i" . indent-region)
         ("C-i" . indent-region)
         ("a" . align-regexp)
         ("A" . align)
         ("e" . er/expand-region)
         ("c" . er/contract-region)
         ("q" . selected-off)
         ("s" . sort-lines)
         ("S" . selected-search)
         ("u" . upcase-region)
         ("d" . downcase-region)
         ("w" . count-words-region)
         ("m" . apply-macro-to-region-lines)
         ("n" . sam-narrow-or-widen-dwim)
         ("x" . kill-ring-save)
         ("X" . kill-region)
         :map selected-org-mode-map
         ("e" . org-insert-structure-template)
         ("t" . org-table-convert-region)
         ("|" . org-table-convert-region)
         :map selected-markdown-mode-map
         ("q" . markdown-blockquote-region))
  :custom
  (selected-minor-mode-override t)
  :init
  (setq selected-org-mode-map (make-sparse-keymap))
  (setq selected-markdown-mode-map (make-sparse-keymap))
  :config
  (selected-global-mode)

  (defun selected-search (key beg end)
    (interactive
     (list
      (read-key (propertize-prompt "\
Search [g]oogle / google-[s]cholar / [w]ikipédia / (C-g) Escape"))
      (region-beginning)
      (region-end)))
    (pcase key
      (?s (sam-google-scholar beg end))
      (?g (sam-google beg end))
      (?w (sam-wikipedia beg end))
      (?\C-g nil)
      (_ (call-interactively 'selected-search)))))


(use-package shell
  :functions (shell-dwim)
  :bind ("s-'" . shell-dwim)
  :commands (shell)
  :config
  (setq explicit-shell-file-name "/usr/local/bin/bash")
  (defun shell-dwim ()
    "Opens a shell buffer in directory associated with current
buffer.

If the current buffer is already a shell buffer, it closes the
window or do nothing if shell is the sole current window in
frame.
"

    (interactive)
    (if (string-equal major-mode "shell-mode")
        (ignore-errors (delete-window))
      (let* ((cwd default-directory)
             (buf "*shell*")
             (proper-cwd (shell-quote-argument (expand-file-name cwd))))
        (shell buf)
        (with-current-buffer buf
          (goto-char (point-max))
          (comint-kill-input)
          (insert (concat "cd " proper-cwd))
          (let ((comint-process-echoes t))
            (comint-send-input))
          (recenter 0))))))


(use-package lisp-mode
  :mode ("\\.cl\\'" . common-lisp-mode))

(use-package slime
  :hook (common-lisp-mode-hook . slime-mode)
  :custom
  (inferior-lisp-program "/usr/local/bin/sbcl --noinform"))


(use-package smartparens
  :diminish (smartparens-mode . "")
  :commands (smartparens-global-mode
             sp-local-pair
	         sp-pair)
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  :bind* (("C-M-f" . sp-forward-sexp)
          ("C-M-b" . sp-backward-sexp)
          ("C-M-d" . sp-down-sexp)
          ("C-M-a" . sp-backward-down-sexp)
          ("C-S-d" . sp-beginning-of-sexp)
          ("C-S-a" . #'sp-beginning-of-sexp)
          ("C-S-e" . sp-end-of-sexp)
          ("C-M-e" . sp-up-sexp)
          ("C-M-u" . sp-backward-up-sexp)
          ("C-M-t" . sp-transpose-sexp)
          ("C-S-r" . sp-forward-slurp-sexp)
          ("C-S-t" . sp-backward-barf-sexp)
          ("C-S-c" . sp-forward-barf-sexp)
          ("C-S-s" . sp-backward-slurp-sexp)
          ("C-{"   . sp-backward-barf-sexp)
          ("C-}"   . sp-slurp-hybrid-sexp)
          ("C-S-b" . sp-backward-symbol)
          ("C-S-f" . sp-forward-symbol)
          ("C-ß"   . sp-splice-sexp)
          ("C-M-S-c" . sp-raise-sexp))
  :hook
  ((after-init . smartparens-global-mode)
   (prog-mode .  smartparens-strict-mode))
  :config
  ;; Only use smartparens in web-mode
  (sp-local-pair 'text-mode "« " " »" :trigger "«" :trigger-wrap "«")

  (sp-local-pair 'cperl-mode "<" ">")

  (sp-local-pair 'markdown-mode "_" "_")
  (sp-local-pair 'markdown-mode "“" "”")
  (sp-local-pair 'markdown-mode "**" "**")
  (sp-local-pair 'markdown-mode "`" "`")
  (sp-local-pair 'markdown-mode "\\<" "\\>")
  (sp-local-pair 'markdown-mode "$" "$")

  (sp-local-pair 'web-mode "<% " " %>")
  (sp-local-pair 'web-mode "{ " " }")
  (sp-local-pair 'web-mode "<%= "  " %>")
  (sp-local-pair 'web-mode "<%# "  " %>")
  (sp-local-pair 'web-mode "<%$ "  " %>")
  (sp-local-pair 'web-mode "<%@ "  " %>")
  (sp-local-pair 'web-mode "<%: "  " %>")
  (sp-local-pair 'web-mode "{{ "  " }}")
  (sp-local-pair 'web-mode "{% "  " %}")
  (sp-local-pair 'web-mode "{%- "  " %}")
  (sp-local-pair 'web-mode "{# "  " #}")

  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'text-mode "--- " " ---" :trigger "—" :trigger-wrap "—")

  (sp-local-pair 'org-mode "/" "/" :actions '(wrap insert))
  (sp-local-pair 'org-mode "*" "*" :actions '(wrap insert) :unless '(sp-point-at-bol-p))

  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)

  (sp-local-pair 'cperl-mode  "{" nil :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'c-mode  "{" nil :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'cc-mode "{" nil :post-handlers '((sam--create-newline-and-enter-sexp "RET")))



  (defun sam--create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    ;; from https://github.com/Fuco1/smartparens/issues/80
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))


(use-package smex)


(use-package swiper
  :bind* (:map swiper-isearch-map
          ("C-s" . #'ivy-next-line)
          ("C-r" . #'ivy-previous-line)
          ("C-t" . #'ivy-yank-word)
          :map global-map
          ("M-s" . #'swiper-isearch)))

(use-package undo-tree
  :demand t
  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize)
         :map undo-tree-visualizer-mode-map
         ("RET" . undo-tree-visualizer-quit)
         ("t" . undo-tree-visualize-redo)
         ("s" . undo-tree-visualize-undo)
         ("c" . undo-tree-visualize-switch-branch-left)
         ("r" . undo-tree-visualize-switch-branch-right))
  :commands (undo-tree-mode
             turn-on-undo-tree-mode
             global-undo-tree-mode)
  :config
  (global-undo-tree-mode 1))


(use-package visual-fill-column
  :bind* (("s-ç" . visual-fill-column-mode))
  :custom
  (visual-fill-column-width 72)
  :config
  (add-hook! 'visual-fill-column-mode-hook
    (visual-line-mode +1)
    (auto-fill-mode -1)))


(use-package xterm-color
  :config
  ;; comint install
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))


(use-package xsv
  :load-path "~/dotfile/emacs/private/xsv/"
  :commands (xsv-mode))


(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode))


(use-package yasnippet
  :defer 5
  :commands (yas-global-mode)
  :config
  (setq yas-indent-line 'none)
  (add-to-list 'yas-snippet-dirs (expand-file-name "~/dotfile/emacs/snippets/"))
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yas-helpers
  :after yasnippet
  :load-path "~/.emacs.d/lisp/")

(use-package fasd
  :after exec-path-from-shell
  :hook
  (after-init . global-fasd-mode)
  :bind
  ("s-r" . #'fasd-find-file))

(use-package rect
  :bind (:map rectangle-mark-mode-map
         ("k" . #'kill-rectangle)
         ("DEL" . #'kill-rectangle)))

(use-package winner
  :hook (after-init . winner-mode)
  :config
  (defhydra hydra-winner (:hint nil)
    ""
    ("u" #'winner-undo "undo")
    ("<left>" #'winner-undo "undo")
    ("r" #'winner-redo "redo")
    ("<right>" #'winner-redo "redo"))
  (define-key winner-mode-map (kbd "C-c <left>") #'hydra-winner/winner-undo)
  (define-key winner-mode-map (kbd "C-c <right>") #'hydra-winner/winner-redo))

(use-package pdf-view
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package spaceline-config
  :config
  (spaceline-spacemacs-theme)
  (spaceline-toggle-minor-modes-off))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (require 'justify-kp)
  (setq nov-text-width most-positive-fixnum)

  (defun sam-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "ETBembo"
                             :height 1.5))
  (add-hook 'nov-mode-hook 'sam-nov-font-setup)

  (defun sam-nov-window-configuration-change-hook ()
    (sam-nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
                 'sam-nov-window-configuration-change-hook
                 t))

  (defun sam-nov-post-html-render-hook ()
    (if (get-buffer-window)
        (let ((max-width (pj-line-width))
              buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (not (looking-at "^[[:space:]]*$"))
                (goto-char (line-end-position))
                (when (> (shr-pixel-column) max-width)
                  (goto-char (line-beginning-position))
                  (pj-justify)))
              (forward-line 1))))
      (add-hook 'window-configuration-change-hook
                'sam-nov-window-configuration-change-hook
                nil t)))

  (add-hook 'nov-post-html-render-hook 'sam-nov-post-html-render-hook))

(use-package plain-org-wiki
  :custom
  (plain-org-wiki-directory "~/wiki"))

;;;; Personnal functions

(use-package sam
  :load-path "~/.emacs.d/lisp/")

(load (no-littering-expand-etc-file-name "custom.el"))

(eval-after-load 'info
  '(add-to-list 'Info-directory-list "/usr/local/share/info"))

(provide 'samuelbarreto)
;;; samuelbarreto.el ends here
