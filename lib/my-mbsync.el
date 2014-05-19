;; -*- lexical-binding: t -*-
(provide 'my-mbsync)

(defvar my-mbsync-process-name "mbsync")
(defvar my-mbsync-process-buffer "*mbsync*")
(defun my-mbsync ()
  (message "Synchronizing mail...")
  (start-process my-mbsync-process-name
		 my-mbsync-process-buffer
		 "mbsync"
		 "--all"))
(defun my-mbsync-set-finish-action (action)
  (set-process-sentinel (get-process "mbsync")
			(lambda (process event)
			  (when (equal event "finished\n")
			    (funcall action)))))
(defun my-mbsync-group ()
  (interactive)
  (my-mbsync)
  (my-mbsync-set-finish-action 'gnus-group-get-new-news))

(defun my-mbsync-summary ()
  (interactive)
  (my-mbsync)
  (my-mbsync-set-finish-action 'gnus-summary-rescan-group))

(defun my-mbsync-update ()
  "To be called when modifying the mailbox in any way, e.g. by changing the marks, or moving mail."
  (my-mbsync)
  (my-mbsync-set-finish-action (lambda () (message "mail sync complete"))))
