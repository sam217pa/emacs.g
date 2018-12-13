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

(provide 'yas-helpers)
