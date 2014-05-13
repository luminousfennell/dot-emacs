(defun my-synctex-goto (filename line col)
  "Move select buffer named (file-name-nondirectory FILENAME) and move point to LINE and COLUMN."
  (find-file filename)
  (goto-line line)
  (when (and (<= 0 col) (<= col (line-end-position)))
    (right-char col)))

(defun my-synctex-zathura ()
  (interactive)
  (let ((line (line-number-at-pos))
	(col (- (point) (line-beginning-position)))
	(pdf-name (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
	(tex-name (buffer-file-name)))
    (call-process "zathura" nil 0 nil "--synctex-forward" (format "%d:%d:%s" line col tex-name) pdf-name)))

(provide 'my-synctex)
