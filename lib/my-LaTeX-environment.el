(provide 'my-LaTeX-environment)

(defun noop ()
  (interactive))
(defun my-LaTeX-environment ()
  (interactive)
  (flet ((LaTeX-insert-environment (environment &optional extra) 
                                   (my-LaTeX-insert-environment environment extra)))
    (call-interactively 'LaTeX-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Newline stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert a newline if finishing a sentence

(defvar my-LaTeX-non-sentence-env-list '("tikzpicture"))

;; TODO what about macros?
(defun my-LaTeX-in-sentence-env-p (point)
  "Test if POINT is in an environment where periods and such end sentences"
  (save-excursion 
    (goto-char point)
    (not (or (texmathp)
             (member (LaTeX-current-environment) my-LaTeX-non-sentence-env-list)))))

(defun my-LaTeX-at-sentence-end-p (point)
  "Test if POINT is at the end of a sentence."
  (and (my-LaTeX-in-sentence-env-p point)
       (save-excursion
         (goto-char point)
         (let ((line-start (save-excursion (beginning-of-line) (point))))
           (skip-chars-backward "\\t " line-start)
           (string-match "[.]" (char-to-string (char-before)))))))

(defun my-LaTeX-break-after-sentence (else-insert)
  (if (my-LaTeX-at-sentence-end-p (point))
      (reindent-then-newline-and-indent)
    (insert-string else-insert)))

(defun my-LaTeX-break-after-sentence-or-space (do-space)
  (interactive "P")
  (if do-space 
      (insert-char (string-to-char " ") 1)
    (my-LaTeX-break-after-sentence " ")))

;; patch insert env, to not FILL MY STUFF
(defun my-LaTeX-insert-environment (environment &optional extra)
  "Insert LaTeX ENVIRONMENT with optional argument EXTRA."
  (let ((active-mark (and (TeX-active-mark) (not (eq (mark) (point)))))
	prefix content-start env-start env-end)
    (when (and active-mark (< (mark) (point))) (exchange-point-and-mark))
    ;; Compute the prefix.
    (when (and LaTeX-insert-into-comments (TeX-in-commented-line))
      (save-excursion
	(beginning-of-line)
	(looking-at
	 (concat "^\\([ \t]*" TeX-comment-start-regexp "+\\)+[ \t]*"))
	(setq prefix (match-string 0))))
    ;; What to do with the line containing point.
    (cond ((save-excursion (beginning-of-line)
			   (looking-at (concat prefix "[ \t]*$")))
	   (delete-region (match-beginning 0) (match-end 0)))
	  ((TeX-looking-at-backward (concat "^" prefix "[ \t]*")
				    (line-beginning-position))
	   (beginning-of-line)
	   (newline)
	   (beginning-of-line 0))
	  ((bolp)
	   (delete-horizontal-space)
	   (newline)
	   (beginning-of-line 0))
	  (t
	   (delete-horizontal-space)
	   (newline 2)
	   (when prefix (insert prefix))
	   (beginning-of-line 0)))
    ;; What to do with the line containing mark.
    (when active-mark
      (save-excursion
	(goto-char (mark))
	(cond ((save-excursion (beginning-of-line)
			       (or (looking-at (concat prefix "[ \t]*$"))
				   (looking-at "[ \t]*$")))
	       (delete-region (match-beginning 0) (match-end 0)))
	      ((TeX-looking-at-backward (concat "^" prefix "[ \t]*")
					(line-beginning-position))
	       (beginning-of-line)
	       (newline)
	       (beginning-of-line 0))
	      (t
	       (delete-horizontal-space)
	       (insert-before-markers "\n")
	       (newline)
	       (when prefix (insert prefix))))))
    ;; Now insert the environment.
    (when prefix (insert prefix))
    (setq env-start (point))
    (insert TeX-esc "begin" TeX-grop environment TeX-grcl)
    (indent-according-to-mode)
    (when extra (insert extra))
    (setq content-start (line-beginning-position 2))
    (unless active-mark
      (newline)
      (when prefix (insert prefix))
      (newline))
    (when active-mark (goto-char (mark)))
    (when prefix (insert prefix))
    (setq env-end (point))
    (insert TeX-esc "end" TeX-grop environment TeX-grcl)
    (end-of-line 0)
    (if active-mark
	(progn
	  (or (assoc environment LaTeX-indent-environment-list)
              (indent-region content-start (line-beginning-position 2)))
	  (set-mark content-start))
      (indent-according-to-mode))
    (save-excursion (beginning-of-line 2) (indent-according-to-mode))
    (TeX-math-input-method-off)
    (run-hook-with-args 'LaTeX-after-insert-env-hooks
			environment env-start env-end)))

