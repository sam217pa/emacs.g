(defun sam--yas-read-file-name ()
  "Return relative file name with completion."
  (dired-make-relative
   (read-file-name "File: ")))

(defun sam--comment-date ()
  (let ((time (format-time-string "[%Y-%m-%d %H:%M:%S]")))
    (format "%s %s\n%s\t" comment-start time comment-start)))

(defun sam--efetch-formats ()
  (let* ((options '(("fasta" "Fasta Sequence")
                    ("genbank" "GenBank Resume")
                    ("gbwithparts" "GenBank with sequence")))
         (col (sam--completion-collection options)))
    (sam--completion-collection-out
     (ivy-read "Choose format :" col))))

(defun sam--export-code ()
  (let* ((options '(("code" "Code only")
                    ("results" "Results only")
                    ("both" "Code and Results")
                    ("none" "None")))
         (col (sam--completion-collection options)))
    (sam--completion-collection-out
     (ivy-read "Choose format :" col))))

(defun sam-yas-perl-next-sub-name ()
  (interactive)
  (save-excursion
    (if (search-forward-regexp "sub +\\([a-z0-9_]+\\)" nil t)
        (match-string 1)
      "")))

(defmacro yas-helper--make-rule ()
  (let ((indent-width (indent-width)))
    `(make-string (- ,fill-column ,indent-width (string-width yas-text)) ?\-)))

(defun yas-helper--make-chunk-rule ()
  (let ((str (read-string "=> ")))
    (mapconcat
     #'identity
     (list
      comment-start
      (make-string
       (- fill-column (indent-width) (+ (string-width str) 3)) ?\-)
      str)
     " ")))


(provide 'yas-helpers)
