(defun my-synctex-goto (filename line col)
  "Move select buffer named (file-name-nondirectory FILENAME) and move point to LINE and COLUMN."
  (let ((buffer-name (file-name-nondirectory filename)))
    (goto-line line buffer-name)
    (when (and (<= 0 col) (<= col (line-end-position)))
      (right-char col))))

(provide 'my-synctex)
