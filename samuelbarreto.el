;;; Defaults
(defmacro add-hook! (hook &rest body)
  "Nicer add-hooking that prevents writing lambdas explicitely.

Add a lamdba containing BODY to hook HOOK."
  (declare (indent 1))
  `(add-hook ,hook
             (lambda () ,@body)))


(use-package no-littering
  :commands (no-littering-expand-var-file-name
             no-littering-expand-etc-file-name)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(defun sam-keyboard! ()
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
     ;; fn key = hyper
     ns-function-modifier 'hyper
     ;; cette touche n'existe p
     ns-right-alternate-modifier nil)
    ))

(defun sam-darwin-defaults! ()
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

(defun sam-environment! ()
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8"))

(defun sam-defaults! ()
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
  (show-paren-mode)
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

(defvar my-font-for-light "DejaVu Sans Mono 11"
    "Font for coding situations.")

(defun sam-appearances! ()
  "Set the default theme and fonts"
  (load-theme 'leuven t)

  (when window-system
    ;; increase space between lines
    (setq-default line-spacing 6)

    ;; change default font for current frame
    (add-to-list 'default-frame-alist `(font . ,my-font-for-light))
    (set-face-attribute 'default nil :font my-font-for-light)))

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
        `(("\\*Buffer List\\*" display-buffer-in-side-window
           (side . top)
           (slot . -1)
           (window-height . 20)
           (preserve-size . (nil . t)) ,sam--parameters)
          ("\\*Tags List\\*" display-buffer-in-side-window
           (side . right)
           (slot . 1)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil)) ,sam--parameters)
          ("\\*\\(?:help\\|grep\\|Completions\\)\\*"
           display-buffer-in-side-window
           (side . bottom)
           (slot . -1)
           (preserve-size . (nil . t))
           ,sam--parameters)
          ("\\*\\(?:shell\\|compilation\\)\\*" display-buffer-in-side-window
           (side . bottom)
           (slot . 1)
           (preserve-size . (nil . t))
           ,sam--parameters)
          ("\\*Org Select\\*" display-buffer-in-side-window
           (side . top)
           (slot . -1)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil))
           ,sam--parameters))))

(add-hook! 'after-init-hook
  (sam--initialize-frame!)
  (sam--windows!)
  (sam-appearances!)
  (sam-environment!)
  (sam-defaults!)
  (sam-keyboard!)
  (sam-darwin-defaults!))


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


(use-package auto-fill-mode
  :diminish auto-fill-function
  :commands turn-on-auto-fill
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill))


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
             counsel-bookmark)
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
                 (window-width  . fit-window-to-buffer)
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

  (bind-keys :map dired-mode-map
    ("SPC" . dired-view-other-window)
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
  :requires elfeed-org
  :functions (elfeed-next-tag
              counsel-elfeed--update-tag)
  :commands (elfeed-update-filter
	     elfeed-search-update
	     elfeed-search-set-filter)
  :bind* (("<f6>" . elfeed)
          ("C-c E" . elfeed)
          :map elfeed-search-mode-map
          ("N" . elfeed-next-tag)
          ("U" . elfeed-search-fetch))
  :config
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
  :bind (:map emacs-lisp-mode-map
              ("ê u" . use-package-jump)))


(use-package ess-site
  :mode ("\\.R\\'" . R-mode))

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
  :bind* (("C-c M-f" . flyspell-buffer)))


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


(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :commands (hl-todo-previous
             hl-todo-next
             hl-todo-occur))


(use-package hungry-delete
  :demand t
  :commands (global-hungry-delete-mode)
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
               hydra-set-transient-map))

(use-package ibuffer
  :bind* (("C-x b" . ibuffer))
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
                                (mode . ess-help-mode)
                                (mode . inferior-ess-mode)))
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


(use-package imake
  :commands (imake))


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
          ("C-x C-i" . ispell-word-then-abbrev))
  :config

  (setq ispell-dictionary "francais")

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
  :commands (ivy-mode
             ivy-read
             ivy-completing-read)
  :defines (use-package-jump)
  :bind* (("C-x C-b" . ivy-switch-buffer)
	      ("s-t" . ivy-switch-buffer)
	      ("s-<backspace>" . ivy-switch-buffer))
  :custom
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  (setq ivy-display-function nil)

  (defun use-package-jump ()
    "Jump to an outer-level `use-package' definition in current buffer."
    (interactive)
    (let ((packages (use-package-jump--list-calls)))
      (goto-char (cdr (assoc (ivy-completing-read "Package: " packages)
                             packages)))))


  (defun sam|genome-accession-numbers ()
    (interactive)
    (let* ((accessions '(("NC_005966.1" "Acinetobacter baylyi ADP1")))
           (cols (sam--completion-collection accessions)))
      (ivy-read "Chose genome ?" cols
                :action (lambda (_) (insert (sam--completion-collection-out _)))))))


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
  :init
  (key-chord-mode 1)
  (setq key-chord-two-key-delay 0.2))


(use-package key-seq
  :after key-chord
  :commands (key-seq-define-global)
  :init
  (key-seq-define-global "qd" #'kill-this-buffer))


(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-list
             lorem-ipsum-insert-sentences
             lorem-ipsum-insert-paragraphs))


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


(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))


(use-package minions
  :hook (after-init . minions-mode)
  :commands (minions-mode))


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
  (org-startup-indented t)
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


(use-package org-babel
  :after org
  :commands (org-babel-tangle))


(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("➡" "➠" "➟" "➝" "↪")))


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

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))


(use-package smartparens
  :diminish (smartparens-mode . "")
  :commands (smartparens-global-mode
	     smartparens-strict-mode
	     sp-local-pair
	     sp-pair)
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
          ("C-S-c" . sp-forward-barf-sexp)
          ("C-S-s" . sp-backward-slurp-sexp)
          ("C-S-t" . sp-backward-barf-sexp)
          ("C-{"   . sp-backward-barf-sexp)
          ("C-}"   . sp-slurp-hybrid-sexp)
          ("C-S-b" . sp-backward-symbol)
          ("C-S-f" . sp-forward-symbol)
          ("C-S-c" . sp-splice-sexp)
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

  (sp-local-pair 'org-mode "/" "/" :trigger-wrap "/" )
  (sp-local-pair 'org-mode "*" "*" :trigger-wrap "*")
  (sp-local-pair 'org-mode "#+BEGIN_QUOTE\n" "#+END_QUOTE\n" :wrap "M-( q")
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


(use-package swiper
  :bind* ("M-s" . swiper))



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

(use-package yasnippet
  :defer 5
  :commands (yas-global-mode)
  :config
  (setq yas-indent-line 'none)
  (add-to-list 'yas-snippet-dirs (expand-file-name "~/dotfile/emacs/snippets/"))
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

;;; Functions

;; (defun sam--read-transparency-input ()
;;   (let* ((msg "(%s) \t Increase [tT] / Decrease [sS] / Escape [C-g]")
;;                 (alpha    (propertize "%s" ))
;;                 (increase (format "Increase %s / "
;;                                   (propertize "[tT]" 'face '(:foreground "red"))))
;;                 (decrease (format "Decrease %s / "
;;                                   (propertize "[sS]" 'face '(:foreground "red"))))
;;                 (escape   (format "Escape %s"
;;                                   (propertize "C-g" 'face '(:foreground "blue")))))
;;     (read-key (format "(%s) \t %s %s %s]"
;;                       (frame-parameter nil 'alpha)
;;                       increase
;;                       decrease
;;                       escape))))

;; (defun sam|transparency (x)
;;   (interactive
;;    (list (let* ((msg "(%s) \t Increase [tT] / Decrease [sS] / Escape [C-g]")
;;                 (alpha    (propertize "%s" ))
;;                 (increase (format "Increase %s / "
;;                                   (propertize "[tT]" 'face '(:foreground "red"))))
;;                 (decrease (format "Decrease %s / "
;;                                   (propertize "[sS]" 'face '(:foreground "red"))))
;;                 (escape   (format "Escape %s"
;;                                   (propertize "C-g" 'face '(:foreground "blue")))))
;;            (read-key (format "(%s) \t Increase [tT] / Decrease [sS] / Escape [C-g]"
;;                                         (frame-parameter nil 'alpha))))))
;;   (cl-flet
;;       ((set-alpha!
;;         (inc)
;;         (let* ((alpha (frame-parameter (selected-frame) 'alpha))
;;                (next-alpha (cond ((not alpha) 100)
;;                                  ((> (- alpha inc) 100) 100)
;;                                  ((< (- alpha inc) 0) 0)
;;                                  (t (- alpha inc)))))
;;           (set-frame-parameter (selected-frame) 'alpha next-alpha)
;;           (call-interactively 'sam|transparency))))
;;     (pcase x
;;       (?t (set-alpha! +1))
;;       (?s (set-alpha! -1))
;;       (?T (set-alpha! +10))
;;       (?S (set-alpha! -10))
;;       (?= (set-frame-parameter
;;            (selected-frame) 'alpha
;;            (read-number "Set to : ")))
;;       (?\C-g nil)
;;       (_ (call-interactively 'sam|transparency)))))


(defun sam|kill-word-at-point (arg)
  (interactive "P")
  (let* ((argp (and arg (= 4 (prefix-numeric-value arg))))
         (beg (beginning-of-thing (if argp 'symbol 'word)))
         (end (end-of-thing (if argp 'symbol 'word))))
    (save-excursion
      (kill-region beg end))))

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(eval-when-compile
  (defun sam|narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p)
           (narrow-to-region (region-beginning)
                             (region-end)))
          ((derived-mode-p 'org-mode)
           ;; `org-edit-src-code' is not a real narrowing
           ;; command. Remove this first conditional if
           ;; you don't want it.
           (cond ((ignore-errors (org-edit-src-code) t)
                  (delete-other-windows))
                 ((ignore-errors (org-narrow-to-block) t))
                 (t (org-narrow-to-subtree))))
          ((looking-at outline-regexp)
           (ignore-errors (outline-narrow-to-subtree)))
          ((derived-mode-p 'latex-mode)
           (LaTeX-narrow-to-environment))
          (t (narrow-to-defun)))))

(defun sam|switch-to-other-buffer ()
  "Switch to other buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

;; from https://github.com/syl20bnr/spacemacs/
(defun sam|open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path (if (eq major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
    (if file-path
        (shell-command (format "open \"%s\"" file-path))
      (message "No file associated to this buffer."))))

(defun iso-timestamp ()
  (concat (format-time-string "%Y-%m-%dT%T")
          ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
           (format-time-string "%z"))))

(defun sam|iterm-here ()
  "Go to present working dir and focus iterm"
  (interactive)
  (let ((dir (shell-quote-argument (expand-file-name default-directory))))
    (do-applescript
     (concat
      " tell application \"iTerm2\"\n"
      "   tell the current session of current window\n"
      (format "     write text \"cd %s\" \n"
              ;; string escaping madness for applescript
              (replace-regexp-in-string "\\\\" "\\\\\\\\" dir))
      "   end tell\n"
      " end tell\n"
      " do shell script \"open -a iTerm\"\n"))))

(defun sam|iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

(defun sam|finder-here ()
  (interactive)
  (let* ((dir default-directory)
         (scr (format " do shell script \"open %s\"\n" dir)))
    (do-applescript scr)))

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun sam|unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; this function is used to append multiple elements to the list 'ox-latex
(defun append-to-list! (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR. The return value is the new value of LIST-VAR."
  (unless (consp elements) (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))

(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)
(setq set-mark-command-repeat-pop t)

(defun sam|set-transparency (inc)
  "Increase or decrease the selected frame transparency"
  (let* ((alpha (frame-parameter (selected-frame) 'alpha))
         (next-alpha (cond ((not alpha) 100)
                           ((> (- alpha inc) 100) 100)
                           ((< (- alpha inc) 0) 0)
                           (t (- alpha inc)))))
    (set-frame-parameter (selected-frame) 'alpha next-alpha)))

(defun hour-minute-timestamp ()
    (format-time-string "%H:%M" (current-time)))
;;;; personnal interactive functions

(defun sam|indent-region ()
  "Indent region "
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end)))
    (indent-region beg end)))

(defun sam|indent-paragraph ()
  "Indent paragraph at point according to mode"
  (interactive)
  (save-excursion
    (mark-paragraph)
    (indent-region (region-beginning) (region-end))))

(defun sam|join-to-next-line ()
  "Join current line to next line."
  (interactive)
  (join-line 4))

(defun sam|duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(defun sam|maximize-window ()
  "Maximize frame on first use, toggle frame fullscreen on second
consecutive use."
  (interactive)
  (let* ((second? (eq last-command this-command))
         (fullscreen (frame-parameter nil 'fullscreen))
         (maximized? (eq 'maximized fullscreen))
         (fullscreen? (eq 'fullboth fullscreen)))
    (cond ((and second? maximized?)
           (toggle-frame-fullscreen))
          (fullscreen?
           (toggle-frame-fullscreen))
          (t
           (toggle-frame-maximized)))))

(defun sam|main-window (&optional frame)
  "Refocus the main editing window.

Delete all side windows at first use ; at second consecutive use
it also delete other normal windows currently active in the
frame."
  (interactive)
  (let* ((frame (window-normalize-frame frame))
         (window--sides-inhibit-check t)
         (sw? (window-with-parameter 'window-side nil frame)))
    (cond ((and (eq last-command this-command) sw?)
           (ignore-errors (window-toggle-side-windows))
           (delete-other-windows))
          (sw?
           (window-toggle-side-windows))
          (t
           (delete-other-windows)))))


;;;; completion help

(defun sam--completion-collection (col)
  (mapcar (lambda (x)
            (concat (propertize (car x) 'font-lock-face '(:foreground "#268bd2"))
                    " => "
                    (propertize (cadr x) 'face 'slanted)))
          col))

(defun sam--completion-collection-out (candidate)
  (substring-no-properties candidate 0 (string-match " => " candidate)))

;;;; use-package completions

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

;;;; org mode enwrapping

(defun org--wrap (block-name beg end)
  (let ((beg-name (format "#+BEGIN_%s\n" (upcase block-name)))
        (end-name (format "#+END_%s\n" (upcase block-name))))
    (save-excursion
      (goto-char end)
      (if (= end (point-at-bol))
          (insert end-name)
        (insert (concat "\n" end-name))))
    (save-excursion
      (goto-char beg)
      (if (= beg (point-at-bol))
          (insert beg-name)
        (insert (concat "\n" beg-name))))))

(defun org-enwrap! (x beg end)
  (interactive
   (list
    (read-key "Wrap with [q]uote, [s]ource, [e]xample, [v]erse ")
    (region-beginning)
    (region-end)))
  (pcase x
    (?q (org--wrap "QUOTE" beg end))
    (?s (org--wrap "SRC" beg end))
    (?e (org--wrap "EXAMPLE" beg end))
    (?v (org--wrap "VERSE" beg end))
    (?\C-g nil)
    (_ (call-interactively 'org-enwrap!))))


;;; keybindings

(use-package bind-key
  :config
  (bind-keys*
   ("ð"       . sam|kill-word-at-point)
   ("C-c v"   . magit-status)
   ("C-S-k"   . kill-whole-line)
   ("C-/"     . complete-symbol)
   ("C-x n"   . sam|narrow-or-widen-dwim)
   ("C-x ="   . balance-windows)
   ("C-x M-c" . compile)

   ("M-/"   . hippie-expand)
   ("M-«"   . beginning-of-buffer)
   ("M-»"   . end-of-buffer)
   ("M-s-n" . forward-paragraph)
   ("M-s-p" . backward-paragraph)
   ("M-SPC" . cycle-spacing)

   ("s-<tab>" . sam|switch-to-other-buffer)
   ("s-d"     . kill-buffer-and-window)
   ("s-I"     . sam|indent-paragraph)
   ("s-j"     . sam|join-to-next-line)
   ("s-o"     . sam|open-in-external-app)
   ("s-q"     . sam|unfill-paragraph)
   ("s-u"     . negative-argument)
   ("s-w"     . sam|main-window)
   ("s-n"     . sam|narrow-or-widen-dwim)

   ("H-w" . sam|maximize-window)
   ("H-l" . sam|duplicate-line)
   ("H-n" . make-frame)
   ("H-u" . revert-buffer)
   ("H-'" . sam|iterm-here)

   ("H-M-p" . scroll-up-command)
   ("H-M-n" . scroll-down-command)
   ("H-M-s" . mark-sexp)))
