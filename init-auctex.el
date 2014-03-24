;; various TeX options
(setq-default TeX-master nil)
(setq
 TeX-auto-save t
 TeX-parse-self t
 TeX-PDF-mode t
 TeX-install-font-lock (quote tex-font-setup)
 TeX-quote-after-quote t
 TeX-source-correlate-mode t)

;; reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; bibtex
(setq bibtex-files (quote (bibtex-file-path)))

;; Outline and fold
(add-hook 'LaTeX-mode-hook (lambda ()
                             (outline-minor-mode t)
                             (visual-line-mode t)))
;; Key bindings
(add-hook 'LaTeX-mode-hook (lambda ()
                             (define-key LaTeX-mode-map (kbd "M-q") 'noop)
                             (define-key LaTeX-mode-map (kbd "C-c C-e") 'my-LaTeX-environment)
                             (define-key LaTeX-mode-map (kbd "SPC") 'my-LaTeX-break-after-sentence-or-space)
                             (set (make-local-variable 'visual-line-fringe-indicators) t)
                             ;; surround triggers
                             (push '(?% . ("$" . "$")) surround-pairs-alist)
                             ))


(defun noop ()
  (interactive))
(defun my-LaTeX-environment ()
  (interactive)
  (flet ((LaTeX-insert-environment (environment &optional extra) 
 
    (call-interactively 'LaTeX-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Newline stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert a newline if finishing a sentence

(defvar my-LaTeX-non-sentence-env-list '("tikzpicture" "lstlistings"))
(defvar my-LaTeX-sentence-end-marker-re "[.]")

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
           (string-match my-LaTeX-sentence-end-marker-re
                         (char-to-string (char-before)))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end newline stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; synctex
(require 'dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun evince-sync-yes-or-no-p (prompt)
  (let ((use-dialog-box t)
        (last-nonmenu-event nil))
    (y-or-n-p prompt)))



(defun evince-sync-find-buffer-visiting (fname)
  (let ((buf (find-buffer-visiting fname)))
    (if (null buf)
        (progn
          (if (evince-sync-yes-or-no-p (format "[Synctex]: open %s? " fname))
              (find-file-noselect fname)
            (error "[Synctex]: %s is not opened..." fname)))
      buf)))

(defun th-evince-sync (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
         (buf (evince-sync-find-buffer-visiting fname))
         (line (car linecol))
         (col (cadr linecol)))    
    (switch-to-buffer buf)
    (goto-line (car linecol))
    (unless (= col -1)
      (move-to-column col))))

(defvar *dbus-evince-signal* nil)

(defun enable-evince-sync ()
  (require 'dbus)
  (when (and
         (eq window-system 'x)
         (fboundp 'dbus-register-signal))
    (unless *dbus-evince-signal*
      (setf *dbus-evince-signal*
            (dbus-register-signal
             :session nil "/org/gnome/evince/Window/0"
             "org.gnome.evince.Window" "SyncSource"
             'th-evince-sync)))))

