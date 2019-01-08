;;; Defaults

(use-package no-littering
  :commands (no-littering-expand-var-file-name
             no-littering-expand-etc-file-name)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


(use-package sam-defaults
  :load-path "~/.emacs.d/lisp/"
  :commands (sam-initialize!)
  :custom
  (sam-font "Iosevka")
  (sam-variable-pitch-font "Input Sans Narrow")
  (sam-theme 'zenburn)
  :init
  (sam-initialize!))

;;; Packages

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
  :config
  (setq aw-keys '(?t ?s ?r ?n ?m ?a ?u ?i ?e))
  (setq aw-background t)
  (setq aw-ignore-current t))

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

(use-package auto-insert
  :commands (auto-insert
             auto-insert-mode))


(use-package avy
  :commands (avy-goto-word-or-subword-1
             avy-goto-word-1
             avy-goto-char-in-line
             avy-goto-line
             avy-goto-char
             avy-resume)
  :config
  (key-seq-define-global "qé" #'avy-resume)
  (key-seq-define-global "qp" #'avy-goto-word-1)
  (key-seq-define-global "qe" #'avy-goto-char-in-line)
  (key-seq-define-global "ql" #'avy-goto-line)
  (setq avy-keys '(?a ?t ?u ?s ?i ?r ?e ?n ?p ?d ?é ?l))
  (setq avy-all-windows nil)
  (setq avy-styles-alist
        '((avy-goto-char-in-line . post)
          (avy-goto-word-or-subword-1 . post)
          (avy-goto-word-1 . pre))))


(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ("s-i" . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))


(use-package buff-menu
  :bind* (("C-x b" . list-buffers)
          :map Buffer-menu-mode-map
          ("RET" . Buffer-menu-other-window))
  :commands (Buffer-menu-toggle-files-only)
  :config
  (add-hook! 'Buffer-menu-mode-hook
    (Buffer-menu-toggle-files-only 1)))


(use-package comment-dwim-2
  :bind* ("M-;" . comment-dwim-2))


(use-package compile
  :commands (compile)
  :bind (:map compilation-mode-map
              ("t" . compilation-next-error)
              ("s" . compilation-previous-error)
              ("r" . compile-goto-error)))



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
        (concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
                "\\|\\.x\\'\\|\\.d\\'\\|\\.o\\'"
                "\\|\\.aux\\'"))

  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  (setq counsel-find-file-at-point t)

  (defun counsel-font ()
    "Change font of current frame"
    (interactive)
    (ivy-read "Chose font :"
              (font-family-list)
              :caller 'counsel-font
              :action (lambda (x) (set-frame-font x)))))


(use-package counsel-projectile
  :commands (counsel-projectile-mode)
  :bind* (("H-p" . counsel-projectile-switch-project)
          ("s-f" . counsel-projectile-find-file)))


(use-package cwl-mode
  :mode ("\\.cwl\\'" . cwl-mode))


(use-package debian-control-mode
  :load-path "/Users/samuelbarreto/dotfile/emacs/private/dcf/"
  :mode (("DESCRIPTION" . debian-control-mode)))


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
             dired-up-directory)
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
   ("O"   . sam|open-in-external-app)
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



(use-package elfeed-org
    :commands (elfeed-org)
    :init
    (setq rmh-elfeed-org-files (list "~/dotfile/emacs/elfeed.org"))
    :config
    (elfeed-org))

(use-package elfeed
  :functions (elfeed-next-tag
              elfeed-mark-all-as-read
              counsel-elfeed--update-tag)
  :commands (elfeed-update-filter
	         elfeed-search-update
	         elfeed-search-set-filter)
  :bind* (("<f6>" . elfeed)
          ("C-c E" . elfeed)
          :map elfeed-search-mode-map
          ("N" . elfeed-next-tag)
          ("R" . elfeed-mark-all-as-read)
          ("U" . elfeed-search-fetch))
  :config
  (elfeed-org)
  ;; increase title width to accomodate papers
  (setq elfeed-search-title-max-width 120)

  (setq elfeed-db-directory "~/dotfile/emacs/elfeed/")

  (defvar counsel-elfeed-tags
    '(("papers" . "+papers")
      ("biology" . "+bio")
      ("bioinfo" . "+bioinfo")
      ("computer science" . "+compsci")
      ("package update" . "+pkg")
      ("rstat" . "+rstat")
      ("emacs" . "+emacs")
      ("news" . "+news")
      ("communisme libertaire" . "+anar")
      ("communisme marxiste" . "+communisme"))
    "This is a list of tag for elfeed filtering.")

  (defun elfeed-update-filter (x)
    (setf elfeed-search-filter
          (concat "@6-months-ago +unread " x))
    (elfeed-search-update :force))

  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (defun elfeed-next-tag ()
    (interactive)
    (let* ((elfeed-tag-list
            (with-current-buffer
                (find-file-noselect (expand-file-name (car rmh-elfeed-org-files)))
              (org-mode)
              (mapcar
               (lambda (x) (concat "+" x))
               (cdr (reverse (mapcar #'car (org-get-buffer-tags)))))))
           (current-tag
            (car (last (split-string elfeed-search-filter))))
           (new-tag
            (cadr (member current-tag elfeed-tag-list))))
      (if new-tag
          (elfeed-update-filter new-tag)
        (elfeed-update-filter (car elfeed-tag-list)))))

  (defun counsel-elfeed--update-tag (x)
    "Update the elfeed filter with the last 10 char of ivy selection."
    (elfeed-search-set-filter
     (concat "@6-months-ago +unread " x)))

  (defun counsel-elfeed-tag ()
    "Shows a list of elfeed tags I like to browse."
    (interactive)
    (ivy-read "%d Elfeed tag: "
              (with-current-buffer
                  (find-file-noselect (expand-file-name (car rmh-elfeed-org-files)))
                (org-mode)
                (mapcar
                 (lambda (x) (concat "+" x))
                 (cdr (reverse (mapcar #'car (org-get-buffer-tags))))))
              :require-match t
              :action #'counsel-elfeed--update-tag
              :update-fn (lambda ()
                           (counsel-elfeed--update-tag (ivy-state-current ivy-last)))
              :caller 'counsel-elfeed-tag
              :sort t))

  (setq-default elfeed-search-filter "@6-months-ago +unread"))


(use-package elisp-mode
  :functions (use-package-jump)
  :bind (:map emacs-lisp-mode-map
              ("A-j" . use-package-jump))
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


(use-package ess
  :mode ("\\.R\\'" . ess-r-mode)
  :custom
  (ess-eval-visibly nil)
  (ess-offset-continued 2)
  (ess-expression-offset 2)
  (ess-nuke-trailing-whitespace-p t)
  (ess-default-style 'RStudio)
  (ess-help-reuse-window t)
  (ess-use-ido nil)
  (ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
                              (ess-R-fl-keyword:constants . t)
                              (ess-R-fl-keyword:modifiers . t)
                              (ess-R-fl-keyword:fun-defs . t)
                              (ess-R-fl-keyword:assign-ops . t)
                              (ess-fl-keyword:fun-calls . t)
                              (ess-fl-keyword:numbers . t)
                              (ess-fl-keyword:operators . t)
                              (ess-fl-keyword:delimiters . t)
                              (ess-fl-keyword:= . t)
                              (ess-R-fl-keyword:F&T . t)
                              (ess-R-fl-keyword:%op% . t)))
  :bind* (:map ess-mode-map
               ("RET" . ess-newline-and-indent)
               ("S-<return>" . ess-eval-line)
               ("C-RET" . ess-eval-region-or-line)
               ("M-RET" . ess-eval-function-or-paragraph)
               ("C-c M-s" . ess-switch-process)
               ("_" . self-insert-command)
               (" " . ess-insert-assign)
               (" " . ess-insert-assign))
  :config
  (add-hook! 'ess-mode-hook
    (setq-local outline-regexp "^## \\*"))

  (add-to-list 'hs-special-modes-alist
               '(ess-r-mode "{" "}" "#[#']" nil nil))



  (sp-local-pair
   'ess-mode "{" nil
   :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair
   'ess-mode "(" nil
   :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'ess-mode "%" "%")
  (sp-local-pair 'ess-mode "`" "`" :actions '(wrap insert) :when '(sp-in-comment-p))

  (defalias 'ess-set-width 'ess-execute-screen-options))


(use-package ess-inf
  :bind (:map inferior-ess-mode-map
         ("_" . self-insert-command)))

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


(use-package flymake
  :commands (flymake-mode
             hydra-errors/body
             flymake-goto-next-error
             flymake-goto-prev-error)
  :functions (hydra-errors/body)
  :config
  (defhydra hydra-errors (:body-pre (flymake-mode)
                          :post (flymake-mode -1)
                          :hint nil)
    "
Linting : [_n_]ext / [_p_]rev
Errors  : [_t_]: next / [_s_]: prev
Totos   : _C-n_: next / _C-p_: prev / _C-s_: search"
    ("n" flymake-goto-next-error)
    ("p" flymake-goto-prev-error)
    ("t" next-error)
    ("s" previous-error)
    ("C-n" hl-todo-next)
    ("C-p" hl-todo-previous)
    ("C-s" hl-todo-occur)))


(use-package flyspell
  :bind* (("C-c M-f" . flyspell-buffer)
          :map flyspell-mode-map
          ("C-c M-n" . flyspell-goto-next-error)))


(use-package flx)


(use-package geiser-install
  :hook (scheme-mode . geiser-mode)
  :custom
  (geiser-active-implementation '(guile chez))
  (geiser-scheme-dir "~/.emacs.d/lib/geiser/scheme"))


(use-package ggo-mode
  :mode ("\\.ggo\\'" . ggo-mode))


(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))


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


(use-package hy-mode
  :mode ("\\.hy\\'" . hy-mode))


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
    ("l" counsel-yank-pop "list" :color blue)))

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
                   ("RStat" (or (mode . ess-mode)
                                (mode . inferior-ess-mode)
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
                   ("Elisp" (mode . emacs-lisp-mode))
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
  (ivy-mode 1))


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

(use-package key-chord
  :commands (key-chord-mode)
  :custom
  (key-chord-two-key-delay 0.2)
  :init
  (key-chord-mode 1))


(use-package keyfreq
  :commands (keyfreq-mode
             keyfreq-autosave-mode)
  :config
  (keyfreq-autosave-mode 1))


(use-package key-seq
  :after key-chord
  :commands (key-seq-define-global
             key-seq-define)
  :init
  (key-seq-define-global "qd" #'sam-ktb)
  (key-seq-define-global "qb" #'counsel-bookmark)
  (key-seq-define-global "qf" #'kill-frame)
  (key-seq-define-global "qw" #'kill-window)
  (key-seq-define emacs-lisp-mode-map "$u" #'use-package-jump))


(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-list
             lorem-ipsum-insert-sentences
             lorem-ipsum-insert-paragraphs))


(use-package lesspy
  :hook (ess-mode . lesspy-mode)
  :bind (:map lesspy-mode-map
         ("a" . lesspy-avy-jump)
         ("p" . lesspy-eval-function-or-paragraph)
         ("h" . lesspy-help)
         ("l" . lesspy-eval-line)
         ("L" . lesspy-eval-line-and-go)
         ("e" . lesspy-eval-sexp)
         ("E" . lesspy-avy-eval)
         ("c" . lesspy-left)
         ("t" . lesspy-down)
         ("s" . lesspy-up)
         ("r" . lesspy-right)
         ("d" . lesspy-different)
         ("m" . lesspy-mark)
         ("x" . lesspy-execute)
         ("u" . lesspy-undo)
         ("z" . lesspy-to-shell)
         ("(" . lesspy-paren)
         ("»" . lesspy-forward-slurp)
         ("«" . lesspy-backward-slurp)
         ("#" . lesspy-comment)
         ("'" . lesspy-roxigen)
         ("C" . lesspy-cleanup-pipeline)
         ("C-d" . lesspy-kill-forward)
         ("C-(" . lesspy-paren-wrap-next)
         ("DEL" . lesspy-kill-backward)))


(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode))


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
        (seq-map
         (lambda (x) (insert (format "\t$(info make %s -- )\n" (car x))))
         targets)))))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :hook (markdown-mode . outline-minor-mode))


(use-package camp
  :load-path "~/dotfile/emacs/private/minimenu/"
  :commands (camp-minor-mode)
  :hook (emacs-lisp-mode . camp-minor-mode))

(use-package minions
  :hook (after-init . minions-mode))


(use-package moody
  :commands (moody-replace-mode-line-buffer-identification
             moody-replace-vc-mode)
  :init
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))


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


(use-package nim-mode
  :mode ("\\.nim\\'" . nim-mode))

(use-package nim-suggest
  :after nim-mode
  :hook ((nim-mode . nimsuggest-mode)
         (nimsuggest-mode . flymake-mode)))


(use-package org
  :commands (org-make-link-string
             org-next-visible-heading
             org-previous-visible-heading
             org-display-inline-images
             org-remove-inline-images
             org-toggle-inline-images
             org-get-buffer-tags)
  :mode (("\\.org\\'" . org-mode)
         ("README\\'"   . org-mode))
  :bind* (("C-c C-w" . org-refile)
          ("C-c C-l" . org-store-link)
          ("C-M-c"   . org-capture)
          :map org-mode-map
          ("s-e"     . org-babel-tangle-all-block-same-file)
          ("s-l"     . org-latex-export-to-latex)
          ("C-c ."   . org-time-stamp)
          ("C-c M-i" . org-insert-link)
          ("C-c m"   . hydra-org-image/body)
          ("C-c $"   . hydra-org-archive/body)
          ("C-c e d" . org-decrypt-entry)
          ("C-c e e" . org-encrypt-entry)
          ("C-c e s" . org-sparse-tree)
          ("C-c e t" . org-tags-sparse-tree))
  :custom
  ;; activate speed commands when on any outline star
  (org-use-speed-commands
   (lambda () (and (looking-at org-outline-regexp)
              (looking-back "^\**" (1- (point))))))
  (org-indirect-buffer-display 'current-window)
  ;; make symlink instead of hard copy
  (org-attach-method 'lns)
  ;; delete attachment when archiving entry
  (org-attach-archive-delete t)
  ;; change folder from data/ to .attach/
  (org-attach-directory ".attach/")
  (org-tags-column 80)
  (org-hide-block-startup t)
  (org-refile-targets '(("~/these/meta/nb/These.org" :level . 2)
                        ("~/Org/TODO" :level . 2)
                        ("~/Org/TODO" :level . 1)
                        ("~/these/meta/nb/maybe.org" :level . 1)
                        ("~/Org/maybe.org" :level . 1)))
  (org-default-notes-file "~/Org/notes.org")

  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-enforce-todo-dependencies t)
  (org-link-abbrev-alist
   '(("wiki" . "https://en.wikipedia.org/wiki/%h")
     ("gsc"  . "https://scholar.google.com/scholar?q=%h")
     ("sep"  . "https://plato.stanford.edu/search/search?query=%h")
     ("etym" . "http://www.cnrtl.fr/etymologie/%h")
     ("bu"   . "http://hola.univ-lyon1.fr/ipac20/ipac.jsp?menu=search&aspect=basic_search&npp=10&ipp=20&spp=20&profile=scd&ri=&index=.GK&term=%h&terms=&valider=Ok")))
  (org-crypt-disable-auto-save t)
  (org-src-preserve-indentation t)
  (org-footnote-auto-adjust t)
  (org-footnote-define-inline nil)
  (org-footnote-fill-after-inline-note-extraction t)
  (org-footnote-section nil)
  (org-export-with-todo-keywords nil)
  (org-export-default-language "fr")
  (org-export-backends '(ascii html icalendar latex md koma-letter))
  (org-export-with-tags nil)
  (org-startup-with-inline-images t)
  (org-startup-indented nil)
  (org-image-actual-width '(400))
  :config

  (setq org-modules '(org-crypt))

  (defhydra hydra-org-archive (:color red :columns 1)
    ("a" org-archive-subtree "archive")
    ("n" org-next-visible-heading "next")
    ("t" org-next-visible-heading "next")
    ("p" org-previous-visible-heading "previous")
    ("s" org-previous-visible-heading "previous"))

  (defhydra hydra-org-image (:color red :hint nil)
    "
Display inline images ?
_y_es  _n_o    _t_oggle
"
    ("y" org-display-inline-images)
    ("n" org-remove-inline-images)
    ("t" org-toggle-inline-images))

  (defun org-babel-tangle-all-block-same-file ()
    "tangle all blocks which belong to the same file."
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively #'org-babel-tangle))))


(use-package org-agenda
  :bind* (("C-c a" . org-agenda))
  :custom (org-agenda-window-setup 'current-window)
  :config

  ;; inspired from  http://pages.sachachua.com/.emacs.d/Sacha.html#orgce6f46d
  (setq org-agenda-files
        (list "~/Org/TODO"
              "~/these/meta/nb/These.org"))

  (setq org-capture-use-agenda-date t) ; when press k from agenda, use agenda date.
  (setq org-agenda-span 7)
  (setq org-agenda-tags-column -100) ; take advantage of the screen width
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-deadline-warning-days 4)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

  ;; agenda start on mondays
  (setq org-agenda-start-on-weekday 1)

  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((agenda "")
            (alltodo "")))
          ("l" "Lab" tags-todo "@these"
           ((org-agenda-files '("~/these/meta/nb/These.org"))
            (org-agenda-sorting-strategy '(timestamp-up priority-up)))
           ("~/these/meta/nb/These.html"))
          ("p" "perso" tags "@perso"
           ((org-agenda-sorting-strategy '(ts-up priority-up))))))

  (setq org-agenda-include-diary nil))


(use-package org-archive
  :after org
  :commands (org-archive-subtree))


(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "◉" "○" "◉" "○")))


(use-package org-capture
  :bind* (("C-M-c" . org-capture))
  :custom
  (org-capture-templates
   '(("t" "these - Todo"     entry (file+headline "~/these/meta/nb/These.org" "InBox"             ) "** %?\n%U")
     ("l" "these - Lab"      entry (file+olp+datetree "~/these/meta/nb/journal.org"               ) "* %(hour-minute-timestamp) %?%^g\n")
     ("r" "these - tickleR"  entry (file+headline "~/these/meta/nb/These.org" "Tickler"           ) "** %?\n%T")
     ("a" "these - rapport"  entry (file+headline "~/these/these/notes-manuscrit.org" "Collecte"  ) "** %?\n%U")
     ("c" "perso - Collecte" entry (file+headline "~/Org/TODO" "Collecte"                         ) "** %?\n%U\n")
     ("n" "perso - Notes"    entry (file+olp+datetree "~/Org/journal.org"                         ) "* %(hour-minute-timestamp) %?%^g\n"))))


(use-package org-download
  :after org
  :bind* (:map org-mode-map
               ("C-c y e" . org-download-edit)
               ("C-c y e" . org-download-edit)
               ("C-c y i" . org-download-image)
               ("C-c y s" . org-download-screenshot)
               ("C-c y y" . org-download-yank)
               ("C-c y k" . org-download-delete))
  :config
  (defvar sam|org-download-dir "./img/"
    "Default folder to place `org-download' captures in.")

  (defun sam|img-dir ()
    (let (target sam|org-download-dir)
      (cond ((file-directory-p target) target)
            (t (make-directory target) target))))

  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir sam|org-download-dir)
  (when (eq system-type 'darwin)
    (setq-default org-download-screenshot-method "screencapture -i %s")))


(use-package org-web-tools
  :commands (org-web-tools-insert-link-for-url
             org-web-tools-insert-web-page-as-entry
             org-web-tools-read-url-as-org
             org-web-tools-convert-links-to-page-entries))


(use-package ob
  :after org
  :commands (org-babel-tangle))


(use-package ob-R
  :commands (org-babel-execute:R))

(use-package ob-perl
  :commands (org-babel-execute:perl))

(use-package ob-python
  :commands (org-babel-execute:python))

(use-package ob-shell
  :commands (org-babel-execute:shell))

(use-package ob-emacs-lisp
  :commands (org-babel-execute:emacs-lisp))

(use-package ob-dot
  :commands (org-babel-execute:dot))

(use-package ob-makefile
  :commands (org-babel-execute:makefile))

(use-package ox-latex
  :commands (org-latex-export-as-latex
             org-latex-export-to-latex)
  :config

  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

  (setq
   ;; moyen d'export latex
   org-latex-pdf-process
   (list "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         "biber %f"
         "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f")
   org-latex-image-default-width "1\\linewidth"
   org-highlight-latex-and-related '(latex entities) ; colore les macro LaTeX
   ;; tufte-handout class by default.
   org-latex-default-class "tant"
   ;; default package list with sensible options
   org-latex-default-packages-alist nil
   org-latex-listings-langs
   '((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp") (c "C") (cc "C++") (fortran "fortran")
     (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby") (html "HTML")
     (xml "XML") (tex "TeX") (latex "[LaTeX]TeX") (shell-script "bash") (gnuplot "Gnuplot")
     (ocaml "Caml") (caml "Caml") (sql "SQL") (sqlite "sql") (makefile "make")
     (R "r"))
   ;; files extensions that org considers as latex byproducts.
   org-latex-logfiles-extensions
   '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx"
     "log" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv" "bbl")
   org-latex-minted-langs '((emacs-lisp "common-lisp")
                            (cc "c++")
                            (cperl "perl")
                            (shell-script "bash")
                            (caml "ocaml")
                            (python "python")
                            (ess "R"))
   org-latex-remove-logfiles t
   org-src-fontify-natively t
   org-latex-tables-booktabs t)

  (append-to-list!
   'org-latex-classes
   '(("tant"
      "\\documentclass[twoside,a4paper,10pt]{tant}
% \\addbibresource{reference.bib}
"
      ;; ("\\part{%s}" . "\\part*{%s}")
      ;; ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("tufte-book"
      "\\documentclass[a4paper, sfsidenotes, justified, notitlepage]{tufte-book}
       \\input{/Users/samuelbarreto/.templates/tufte-book.tex}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("tufte-handout"
      "\\documentclass[a4paper, justified]{tufte-handout}
       \\input{/Users/samuelbarreto/.templates/tufte-handout.tex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("rapport" "\\documentclass[11pt, oneside]{scrartcl}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass[presentation]{beamer}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("journal"
      "\\documentclass[9pt, oneside, twocolumn]{scrartcl}
       \\input{/Users/samuelbarreto/.templates/journal.tex}"
      ("\\part{%s}" . "\\section*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")))))

(use-package ox-beamer
  :commands (org-beamer-mode))


(use-package outline
  :custom
  (outline-minor-mode-prefix (kbd "C-."))
  :config
  (define-key
    outline-minor-mode-map
    (kbd "TAB")
    '(menu-item "" nil
                :filter (lambda (&optional _)
                          (when (outline-on-heading-p)
                            'bicycle-cycle)))))

;; (use-package outorg
;;   :after outline)

;; (use-package outshine
;;   :hook (outline-minor-mode . outshine-hook-function)
;;   :bind* (:map outline-minor-mode-map
;;                ("C-A-i" . outshine-cycle-buffer))
;;   :custom
;;   (outshine-use-speed-commands t))

;; (use-package navi-mode
;;   :after outline)


(use-package paren
  :hook (after-init . show-paren-mode))


(use-package pathify
  :after dired
  :bind* (:map dired-mode-map
               ("L" . pathify-dired))
  :custom
  (pathify-directory "~/.local/bin/"))


(use-package poporg
  :bind* (("C-c #" . poporg-dwim)))


(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))


(use-package projectile
  :commands (projectile-mode
             projectile-find-file)
  :bind* (("s-p" . projectile-switch-project))
  :custom
  (projectile-switch-project-action 'projectile-dired)
  (projectile-completion-system 'ivy))


(use-package rainbow-mode
  :commands (rainbow-mode))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package reftex
  :after tex-site
  ;; :bind (:map latex-mode-map
  ;;             ("C-c [" . reftex-citation))
  :hook (TeX-latex-mode . reftex-mode)
  :custom
  (reftex-default-bibliography
   '("/Users/samuelbarreto/Dropbox/bibliography/references.bib"))
  (reftex-cite-format 'biblatex))


(use-package selected
  :demand t
  :commands (selected-global-mode)
  :functions (org--wrap
              org-enwrap!
              selected-search)
  :bind (:map selected-keymap
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
         ("n" . sam|narrow-or-widen-dwim)
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
Search [g]oogle / google-[s]cholar / [w]ikipédia /
(C-g) Escape"))
      (region-beginning)
      (region-end)))
    (pcase key
      (?s (sam|google-scholar beg end))
      (?g (sam|google beg end))
      (?w (sam|wikipedia beg end))
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

(use-package slime
  :custom
  (inferior-lisp-program "/usr/local/bin/sbcl --noinform"))


(use-package smartparens
  :diminish (smartparens-mode . "")
  :commands (smartparens-global-mode
	         smartparens-strict-mode
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
          ("C-S-a" . sp-end-of-sexp)
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
          ("C-ß"   . sp-splice-sexp))
  :init
  (add-hook! 'after-init-hook
    (smartparens-global-mode))
  (add-hook! 'prog-mode-hook
    (smartparens-strict-mode))

  :config
  ;; Only use smartparens in web-mode
  (sp-local-pair 'text-mode "« " " »" :trigger "«" :trigger-wrap "«")

  (sp-local-pair 'markdown-mode "_" "_")
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
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'text-mode "--- " " ---" :trigger "—" :trigger-wrap "—")

  (sp-local-pair 'org-mode "/" "/" :actions '(wrap insert))
  (sp-local-pair 'org-mode "*" "*" :actions '(wrap insert) :unless '(sp-point-at-bol-p))

  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)

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


(use-package solarized
  :demand t)


(use-package spotlight
  :bind* (("C-c C-s" . spotlight)
          ("C-c C-S-s" . spotlight-fast))
  :custom
  (spotlight-tmp-file (no-littering-expand-var-file-name "spotlight-tmp-file")))


(use-package string-edit
  :bind* ("C-c s" . string-edit-at-point))


(use-package swiper
  :bind* ("M-s" . swiper))


(use-package tex-site
  :commands (TeX-tex-mode)
  :mode (("\\.tex\\'" . TeX-tex-mode)
         ("\\.cls\\'" . TeX-tex-mode))
  :custom
  (TeX-PDF-mode t)
  (TeX-engine 'default)
  (LaTeX-item-indent 2))


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
  :commands (visual-fill-column-mode)
  :init
  (add-hook! 'visual-fill-column-mode-hook
    (visual-line-mode +1)
    (auto-fill-mode -1)))


(use-package which-key
  :defer 2
  :diminish which-key-mode
  :commands (which-key-mode
             which-key-setup-side-window-bottom
             which-key-add-key-based-replacements)
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  ;; simple then alphabetic order.
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (setq which-key-popup-type 'side-window
        which-key-side-window-max-height 0.3
        which-key-side-window-max-width 0.5
        which-key-idle-delay 0.3
        which-key-min-display-lines 7)
  ;; key description for C-x
  (which-key-add-key-based-replacements
    "C-x RET" "coding system -input"
    "C-x 4"   "Other Window"
    "C-x 5"   "Frame"
    "C-x 6"   "2C"
    "C-x @"   "event"
    "C-x 8"   "special char"
    "C-x a"   "abbrev"
    "C-x n"   "narrow"
    "C-x r"   "rectangle"
    "C-x v"   "version control"
    "C-c &"   "yas"
    "C-c @"   "hide-show"
    "M-SPC h" "info"
    "M-SPC g" "grep"
    "M-SPC M-s" "occur") )

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


(use-package winner
  :hook (after-init . winner-mode)
  :commands (winner-undo
             winner-redo)
  :bind (("C-c <left>" . hydra-winner/winner-undo)
         ("C-c <right>" . hydra-winner/winner-redo))
  :config
  (defhydra hydra-winner (:color red :hint nil)
    ("<left>" winner-undo "undo")
    ("<right>" winner-redo "redo")))


(use-package zenburn-theme
  :no-require t
  :defer t)

;;; Personnal functions

(use-package sam-helpers
  :load-path "~/.emacs.d/lisp/"
  :demand t
  :commands (use-package-jump)
  :bind* (("H-'"     . sam|iterm-here)
          ("H-l"     . sam|duplicate-line)
          ("H-o"     . sam|reveal-in-finder)
          ("H-w"     . sam|maximize-window)
          ("s-<tab>" . sam|switch-to-other-buffer)
          ("s-I"     . sam|indent-paragraph)
          ("s-j"     . sam|join-to-next-line)
          ("s-n"     . sam|narrow-or-widen-dwim)
          ("s-o"     . sam|open-in-external-app)
          ("s-q"     . sam|unfill-paragraph)
          ("s-w"     . sam|main-window)
          ("ð"       . sam|kill-word-at-point)
          ("s-C"     . sam-switch-to-compilation)
          ("C-x n"   . sam|narrow-or-widen-dwim)))

;;; keybindings

(use-package bind-key
  :config
  (bind-keys*
   ("C-/"           . complete-symbol)
   ("C-S-k"         . kill-whole-line)
   ("C-x |"         . split-window-right)
   ("C-x ="         . balance-windows)
   ("C-x M-c"       . compile)
   ("M-SPC"         . cycle-spacing)
   ("M-«"           . beginning-of-buffer)
   ("M-»"           . end-of-buffer)
   ("M-s-n"         . forward-paragraph)
   ("M-s-p"         . backward-paragraph)
   ("s-c"           . clone-indirect-buffer-other-window)
   ("s-d"           . kill-buffer-and-window)
   ("s-u"           . negative-argument)
   ("H-n"           . make-frame)
   ("H-u"           . revert-buffer)
   ("H-<backspace>" . switch-to-buffer-other-window)
   ("H-M-p"         . scroll-up-command)
   ("H-M-n"         . scroll-down-command)
   ("H-M-s"         . mark-sexp)))

(add-hook! 'after-init
  (load (no-littering-expand-etc-file-name "custom.el")))
