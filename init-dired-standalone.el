(load-file "~/.emacs.d/init-common.el")
(load-file "~/.emacs.d/init-dired.el")

(require 'dired-dtach "~/.emacs.d/dired-dtach.el")

;; some special keymaps for dired standalone
(add-hook 'dired-mode-hook (lambda ()
                             (evil-define-key 'normal dired-mode-map "\C-m" 'dired-dtach-find-file)
                             (define-key dired-mode-map (kbd "C-c t")  'dired-dtach-open-terminal)
			     ;; TODO fix ido-gvfs... use dtach
                             ;; (define-key dired-mode-map (kbd "C-x C-f") 'ido-find-file-gvfs)
                             (define-key 
                               dired-mode-map 
                               (kbd "C-u +") 
                               'my-dired-new-file))
	  t)


;; new empty file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; TODO replace this with find-file handler (like gvfs-ido before)
(defun my-dired-new-file (fname)
  (interactive "FTouch new file: ")
  (if (file-exists-p fname)
      (error "File `%s' already exists!" fname)
    (let* ((errfile (make-temp-file "dired-touch-error"))
           (exit-code (call-process "touch" 
                                    nil 
                                    `(nil ,errfile) 
                                    nil 
                                    (expand-file-name fname))))
      (when (not (= 0 exit-code))
        (with-temp-buffer
          (insert-file-contents errfile)
          (error "Error executing `touch': %s" (buffer-string))))))
  (revert-buffer)
  (let ((line 0)        
        (last-line (save-excursion (end-of-buffer) (line-number-at-pos))))
    (save-excursion
      (beginning-of-buffer)
      (while (and (not (string= fname (dired-file-name-at-point)))
                  (not (= (line-number-at-pos) last-line)))
        (dired-next-line 1)
        (setq line (+ 1 line)))
      (if (dired-file-name-at-point)
          (progn (message "%d" line) line)
        (error "Unable to find created file `%s'" real-fname)))
    ;; now `line' is the number of next operations needed
    (beginning-of-buffer)
    ; this is necessary, as `(dired-next-line 1)' somehow behaves
    ; differently than `(dired-next-line x)' where x > 1
    (dotimes (tmp line) (dired-next-line 1))))
