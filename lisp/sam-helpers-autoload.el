
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

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sam-helpers" '(#("use-package-jump--list-calls" 0 28 (face font-lock-function-name-face)) #("sam--" 0 5 (face font-lock-function-name-face)) #("hour-minute-timestamp" 0 21 (face font-lock-function-name-face)) #("modi/multi-pop-to-mark" 0 17 (face font-lock-function-name-face) 17 22 (face font-lock-function-name-face)) #("append-to-list!" 0 15 (face font-lock-function-name-face)) #("iso-timestamp" 0 13 (face font-lock-function-name-face)) #("propertize-prompt" 0 17 (face font-lock-function-name-face fontified nil)))))

;;;***
