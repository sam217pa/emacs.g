;;; sam-defaults-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "sam-defaults" "sam-defaults.el" (0 0 0 0))
;;; Generated autoloads from sam-defaults.el

(autoload 'add-hook! "sam-defaults" "\
Nicer add-hooking that prevents writing lambdas explicitely.

Add a lamdba containing BODY to hook HOOK.

\(fn HOOK &rest BODY)" nil t)

(function-put 'add-hook! 'lisp-indent-function '1)

(autoload 'sam-initialize! "sam-defaults" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sam-defaults" '(#("system-move-file-to-trash" 0 1 (fontified t face font-lock-function-name-face) 1 25 (fontified nil)) #("sam-" 0 1 (fontified t face font-lock-function-name-face) 1 4 (face font-lock-function-name-face fontified t)))))

;;;***

;;;### (autoloads nil "sam-helpers" "sam-helpers.el" (0 0 0 0))
;;; Generated autoloads from sam-helpers.el

(autoload 'sam|adjust-alpha "sam-helpers" "\
Adjust the frame transparence.

- Decrease with fine or coarse grain with s and S,
- Increase with fine or coarse grain with t and T.
- Set to X with =.
- Reset with 0.
- Escape with C-g.

\(fn X)" t nil)

(autoload 'sam|kill-word-at-point "sam-helpers" "\


\(fn ARG)" t nil)

(autoload 'sam|narrow-or-widen-dwim "sam-helpers" "\
Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed.

\(fn P)" t nil)

(autoload 'sam|switch-to-other-buffer "sam-helpers" "\
Switch to other buffer

\(fn)" t nil)

(autoload 'sam|open-in-external-app "sam-helpers" "\
Open current file in external application.

\(fn)" t nil)

(autoload 'sam|reveal-in-finder "sam-helpers" "\
Reveal current file in the finder application.

\(fn)" t nil)

(autoload 'sam|iterm-here "sam-helpers" "\
Go to present working dir and focus iterm

\(fn)" t nil)

(autoload 'sam|iterm-focus "sam-helpers" "\


\(fn)" t nil)

(autoload 'sam|finder-here "sam-helpers" "\


\(fn)" t nil)

(autoload 'sam|unfill-paragraph "sam-helpers" "\
Takes a multi-line paragraph and makes it into a single line of text.

\(fn &optional REGION)" t nil)

(autoload 'sam|indent-region "sam-helpers" "\
Indent region

\(fn)" t nil)

(autoload 'sam|indent-paragraph "sam-helpers" "\
Indent paragraph at point according to mode

\(fn)" t nil)

(autoload 'sam|join-to-next-line "sam-helpers" "\
Join current line to next line.

\(fn)" t nil)

(autoload 'sam|duplicate-line "sam-helpers" "\
Duplicate the line containing point.

\(fn)" t nil)

(autoload 'sam|maximize-window "sam-helpers" "\
Maximize frame on first use, toggle frame fullscreen on second
consecutive use.

\(fn)" t nil)

(autoload 'sam|main-window "sam-helpers" "\
Refocus the main editing window.

Delete all side windows at first use ; at second consecutive use
it also delete other normal windows currently active in the
frame.

\(fn &optional FRAME)" t nil)

(autoload 'sam|google-scholar "sam-helpers" "\


\(fn BEG END)" t nil)

(autoload 'sam|google "sam-helpers" "\


\(fn BEG END)" t nil)

(autoload 'sam-screenshot-theme! "sam-helpers" "\


\(fn)" t nil)

(autoload 'sam-the-the "sam-helpers" "\
Search forward for for a duplicated word.

\(fn)" t nil)

(autoload 'sam-font-mono "sam-helpers" "\
Set the default font to `sam-font'

\(fn)" t nil)

(autoload 'sam-pdftable "sam-helpers" "\


\(fn BUF)" t nil)

(autoload 'sam-circos "sam-helpers" "\


\(fn)" t nil)

(autoload 'sam-load-theme "sam-helpers" "\


\(fn)" t nil)

(autoload 'sam-ktb "sam-helpers" "\
Kill the current buffer without asking for it first.

\(fn)" t nil)

(autoload 'sam-pandoc-samartcl "sam-helpers" "\


\(fn)" t nil)

(autoload 'sam-ibio "sam-helpers" "\
Open a dired buffer into my ibio session

\(fn)" t nil)

(autoload 'sam-switch-to-compilation "sam-helpers" "\
Switch to the current compilation buffer

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sam-helpers" '("hour-minute-timestamp" "helm--load-theme-action" "sam" "modi/multi-pop-to-mark" #("append-to-list!" 0 15 (face font-lock-function-name-face)) #("iso-timestamp" 0 13 (face font-lock-function-name-face fontified t)) #("propertize-prompt" 0 17 (face font-lock-function-name-face fontified t)))))

;;;***

(provide 'sam-defaults-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sam-defaults-autoloads.el ends here
