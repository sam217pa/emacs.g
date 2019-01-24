;;; sam-helpers.el --- personnal helper functions    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: convenience, functions

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

(require 'cl-lib)
(require 'sam-utils)

;;;; Custom

(defcustom sam-font-text "CMU Concrete 16"
  "Font for text situation"
  :group 'sam
  :type 'string)

(defcustom sam-query-urls
  '((wiki    . "https://fr.wikipedia.org/")
    (scholar . "https://scholar.google.com/scholar?q=")
    (google  . "https://www.google.com/search?hl=fr&q="))
  "Defaults url for querying the web."
  :type 'alist
  :group 'sam)

;;;; Functions

(declare-function LaTeX-narrow-to-environment "auctex") ; silence byte-compiler

;;;; propertize prompt
;; A function to propertize a prompt with simple cookies, like it's done
;; in the mu4e package.

(defun propertize-prompt (prompt)
    "Add text properties to special cookies in PROMPT.

Highlight text between square brackets with `highlight',
between round brackets with `font-lock-builtin-face',
between curly brackets with `font-lock-constant-face'."
    (let ((str prompt)
          (regexp-colors '(("\\[[-_A-Za-z0-9=]+\\]" . 'highlight)
                           ("([-_A-Za-z0-9=]+)"     . 'font-lock-builtin-face)
                           ("{[-_A-Za-z0-9=]+}"     . 'font-lock-constant-face))))
      (cl-loop for re-color in regexp-colors do
               (let ((start 0))
                 (while (string-match (car re-color) str start)
                   (let ((beg (1+ (match-beginning 0)))
                         (end (1- (match-end 0))))
                     (setq start end)
                     (add-text-properties
                      beg end
                      `(face ,(cdr re-color))
                      str)))))
      str))

;;; frame transparency adjustment

(defun sam--set-alpha! (inc)
  (let* ((alpha (frame-parameter (selected-frame) 'alpha))
         (next-alpha (cond ((not alpha) 100)
                           ((> (- alpha inc) 100) 100)
                           ((< (- alpha inc) 0) 0)
                           (t (- alpha inc)))))
    (set-frame-parameter (selected-frame) 'alpha next-alpha)
    (call-interactively 'sam-adjust-alpha)))

;;;###autoload
(defun sam-adjust-alpha (x)
  "Adjust the frame transparence.

- Decrease with fine or coarse grain with s and S,
- Increase with fine or coarse grain with t and T.
- Set to X with =.
- Reset with 0.
- Escape with C-g."
  (interactive
   (list
    (read-key
     (propertize-prompt
      (concat (format "(%s)" (frame-parameter (selected-frame) 'alpha))
              " Increase [tT] / Decrease [sS] / Set-to [=] / Reset [0] / (C-g) Escape")))))
  (pcase x
    (?t (sam--set-alpha! +1))
    (?s (sam--set-alpha! -1))
    (?T (sam--set-alpha! +10))
    (?S (sam--set-alpha! -10))
    (?= (set-frame-parameter
         (selected-frame) 'alpha
         (read-number "Set to : ")))
    (?0 (set-frame-parameter (selected-frame) 'alpha 100))
    (?\C-g nil)
    (_ (call-interactively 'sam-adjust-alpha))))

;;;###autoload
(defun sam-kill-word-at-point (arg)
  (interactive "P")
  (let* ((argp (and arg (= 4 (prefix-numeric-value arg))))
         (beg (beginning-of-thing (if argp 'symbol 'word)))
         (end (end-of-thing (if argp 'symbol 'word))))
    (save-excursion
      (kill-region beg end))))

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html

;;;; narrow or widen DWIM

;;;###autoload
(defun sam-narrow-or-widen-dwim (p)
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
        (t (narrow-to-defun))))

;;;###autoload
(defun sam-switch-to-other-buffer ()
  "Switch to other buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

;; from https://github.com/syl20bnr/spacemacs/
;;;; open file in external app

;;;###autoload
(defun sam-open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path (cond ((eq major-mode 'dired-mode)
                          (dired-get-file-for-visit))
                         ((ffap-file-at-point))
                         (t
                          (buffer-file-name)))))
    (if file-path
        (shell-command (format "open \"%s\"" file-path))
      (message "No file associated to this buffer."))))

;;;###autoload
(defun sam-reveal-in-finder ()
  "Reveal current file in the finder application."
  (interactive)
  (let* ((file-path (if (eq major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                      (buffer-file-name)))
         (cmd (format "open -R %s" (shell-quote-argument file-path))))
    (if file-path
        (shell-command cmd)
      (user-error "Buffer is not associated to a file"))))

(defun iso-timestamp ()
  (let ((d (format-time-string "%Y-%m-%dT%T"))
        (z (format-time-string "%z")))
    (concat d (substring z 0 3) ":" (substring z 3 5))))

;;;###autoload
(defun sam-iterm-here ()
  "Go to present working dir and focus iterm"
  (interactive)
  (let*
      ((dir (expand-file-name default-directory))
       (cmd (if (string-match "ssh" dir)
                (let* ((sp-dir (split-string dir ":"))
                       (host (elt sp-dir 1))
                       (-dir (shell-quote-argument (elt sp-dir 2))))
                  (format "ssh -t %s 'cd %s && exec bash -l'" host -dir))
              (format "cd %s" (shell-quote-argument dir)))))
    (do-applescript
     (concat
      " tell application \"iTerm2\"\n"
      "   tell the current session of current window\n"
      (format "     write text \"%s\" \n"
              ;; string escaping madness for applescript
              (replace-regexp-in-string "\\\\" "\\\\\\\\" cmd))
      "   end tell\n"
      " end tell\n"
      " do shell script \"open -a iTerm\"\n"))))

;;;###autoload
(defun sam-iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

;;;###autoload
(defun sam-finder-here ()
  (interactive)
  (let* ((dir default-directory)
         (scr (format " do shell script \"open %s\"\n" dir)))
    (do-applescript scr)))

;;;; unfill paragraph

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;;###autoload
(defun sam-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;; multiple add-to-list

;; this function is used to append multiple elements to the list 'ox-latex
(defun append-to-list! (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR. The return value is the new value of LIST-VAR."
  (unless (consp elements) (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

;;;; pop to mark cycling
(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (cl-loop repeat 10 do
             (when (= p (point))
               (apply orig-fun args)))))

(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)
(setq set-mark-command-repeat-pop t)

(defsubst hour-minute-timestamp ()
  (format-time-string "%H:%M" (current-time)))

;;;; personnal interactive functions

;;;###autoload
(defun sam-indent-region (beg end)
  "Indent region "
  (interactive "r")
  (indent-region beg end))

;;;###autoload
(defun sam-indent-paragraph ()
  "Indent paragraph at point according to mode"
  (interactive)
  (save-excursion
    (mark-paragraph)
    (indent-region (region-beginning) (region-end))))

;;;###autoload
(defun sam-join-to-next-line ()
  "Join current line to next line."
  (interactive)
  (join-line 4))

;;;###autoload
(defun sam-duplicate-line ()
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

;;;###autoload
(defun sam-maximize-window ()
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

;;;###autoload
(defun sam-main-window (&optional frame)
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

(defun sam--query-url (url)
  (lambda (beg end)
    (let* ((q-string (buffer-substring-no-properties beg end))
           (query (url-hexify-string q-string))
           (url-query (concat url query)))
      (browse-url url-query))))



;;;###autoload
(defun sam-google-scholar (beg end)
  (interactive "r")
  (let ((q (alist-get 'scholar sam-query-urls)))
    (funcall (sam--query-url q) beg end)))

;;;###autoload
(defun sam-google (beg end)
  (interactive "r")
  (let ((q (alist-get 'google sam-query-urls)))
    (funcall (sam--query-url q) beg end)))

(defun sam-wikipedia (beg end)
  (interactive "r")
  (let ((q (alist-get 'wiki sam-query-urls)))
    (funcall (sam--query-url q) beg end)))

;;;; completion help

(defun sam--completion-collection (col)
  (mapcar (lambda (x)
            (concat (propertize (car x) 'font-lock-face '(:foreground "#268bd2"))
                    " => "
                    (propertize (cadr x) 'face 'slanted)))
          col))

(defun sam--completion-collection-out (candidate)
  (substring-no-properties candidate 0 (string-match " => " candidate)))

;;;###autoload
(defun sam-screenshot-theme! ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'leuven)
  (set-face-attribute 'nobreak-space nil :background "#f0ffff")
  (set-face-attribute 'default nil :font sam-font-text))

;;;###autoload
(defun sam-the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (save-excursion
    (goto-char 0)
    (push-mark)
    ;; This regexp is not perfect
    ;; but is fairly good over all:
    (if (re-search-forward
         "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
        (message "Found duplicated word.")
      (message "No duplicated word in buffer"))))

;;;###autoload
(defun sam-font-mono ()
  "Set the default font to `sam-font'"
  (interactive)
  (set-face-attribute 'default nil :font sam-font))

;;;###autoload
(defun sam-pdftable (buf)
  (interactive (list (current-buffer)))
  (unless (eq major-mode 'latex-mode)
    (user-error "This function is only available in latex modes."))
  (let* ((bfn (buffer-file-name buf))
         (cmd (format "pdflatex -shell-escape %s" bfn)))
    (compile cmd)))

;;;###autoload
(defun sam-circos ()
  (interactive)
  (unless (member "circos.conf" (directory-files "."))
    (user-error "No circos.conf found in current directory."))
  (let* ((cmd (format "circos")))
    (compile cmd)))

;;;###autoload
(defun sam-ktb ()
  "Kill the current buffer without asking for it first."
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
(defun sam-pandoc-samartcl ()
  (interactive)
  (when (eq major-mode 'markdown-mode)
    (let* ((bfn (buffer-file-name (current-buffer)))
           (out (concat (file-name-sans-extension bfn) ".pdf"))
           (cmd (format "pandoc --template=samartcl -i %s -o %s" bfn out)))
      (compile cmd))))

;;;###autoload
(defun sam-ibio ()
  "Open a dired buffer into my ibio session"
  (interactive)
  (find-file "/ssh:samuel.barreto@ibio.univ-lyon1.fr:/home/pers/samuel.barreto/"))

;; TODO: make this work over tramp or something
(defun sam-rsync-ibio (fap)
  (interactive
   (list
    (dired-filename-at-point)))
  (let ((tgt "samuel.barreto@umr5557-baloo.univ-lyon1.fr:/home_nfs/pers/samuel.barreto/dump/"))
    (message (format "rsync -avz -e ssh %s %s" fap tgt))))

;;;###autoload
(defun sam-switch-to-compilation ()
  "Switch to the current compilation buffer"
  (interactive)
  (switch-to-buffer "*compilation*"))

(defun sam--empty-line? (arg)
  (save-excursion
    (forward-line arg)
    (and (bolp) (eolp))))

(defun sam-brush-up ()
  "Really open a new line."
  (interactive)
  (unless (eolp) (end-of-line))
  (cond ((sam--empty-line? +1)
         (forward-line 1)
         (sam-brush-up))
        ((not (sam--empty-line? -1))
         (newline)
         (sam-brush-up))
        (t (open-line 1))))

(sam-defaliases
  'change-log-add-entry #'add-change-log-entry
  'change-log-add-entry-other-window #'add-change-log-entry-other-window)

(defun sam-defill-paragraph ()
  (interactive)
  (save-excursion
    (sam-unfill-paragraph)
    (save-restriction
      (mark-paragraph)
      (narrow-to-region (point) (mark))
      (while (not (eobp))
        (forward-sentence)
        (when (sam--empty-line? 1)
          (newline-and-indent))))))

(defun sam-defill-buffer ()
  (interactive)
  (while (not (eobp))
    (sam-defill-paragraph)
    (forward-paragraph)))

;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
(defun sam-lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(provide 'sam-helpers)
;;; sam-helpers.el ends here
