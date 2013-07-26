;; (setq user-full-name "Luminous Fennell")
;; (setq user-mail-address "mstrlu@gmx.net")
;; (setq read-mail-command (quote gnus))
;; (setq mail-user-agent (quote gnus-user-agent))

;; BBDB settings
(when (require 'bbdb nil t)
  ;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases)
  (add-hook 'mail-mode-hook 'mail-abbrevs-mode))

;; (setq message-send-mail-function (quote smtpmail-send-it))
;; (setq smtpmail-smtp-service 587)

;; ;; SMTP settings for mstrlu@gmx.net
;; (setq smtpmail-smtp-server "mail.gmx.net")
;; (setq smtpmail-local-domain "gmx.net")
;; ;; (setq smtpmail-auth-credentials
;; ;;       (quote (("mail.gmx.net" 587 "mstrlu@gmx.net" nil))))
;; (setq smtpmail-auth-credentials "~/.authinfo")
;; (setq smtpmail-starttls-credentials (quote (("mail.gmx.net" 587 "" ""))))

;; enable imap searching for gnus
(require 'nnir)

;; ask before sending
(defun my-message-field-entry (field)
  (check-type field string)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (goto-char 1)
      (when (re-search-forward (format "^%s:" field) t)        
        (let ((start (point))
              (end (save-excursion
                     (re-search-forward (format "^[]" field))
                     (previous-line)
                     (end-of-line)
                     (point))))
          (buffer-substring start end))))))

;; (defun my-message-receipient-list ()



  
(defun my-maybe-send-message (&optional arg)
  "Ask for confirmation, before sending a message if arg is nil"
  (interactive "P")
  (when (or arg
            (y-or-n-p "Really send message?"))
    (call-interactively 'message-send-and-exit)))


;; mailfilter editing function
(defvar my-mailfilter-server "machinec.dynalias.net")
(defvar my-mailfilter-testmsg "~/.emacs.d/mailfilter-test.msg")
(defvar my-mailfilter-file (format "/%s:~/.mailfilter" my-mailfilter-server))
(defvar my-mailfilter-test-file "mailfilter.test")
(defvar my-mailfilter-test-file-remote )
(defvar my-mailfilter-test-buffer "*mailfilter-test*")

(defun my-mailfilter ()
  (interactive)
  (find-file my-mailfilter-file)
  (add-hook 'before-save-hook 'my-mailfilter-check nil t))

(defun my-mailfilter-check ()
  (let ((cur-buf (current-buffer)))
    ;; copy contents to a new buffer to avoid the modified hook
    (with-temp-file (format "/%s:~/%s"
                            my-mailfilter-server
                            my-mailfilter-test-file)
      (insert-buffer cur-buf)))
  ;; check fot syntax errors
  (with-temp-buffer
    (let ((res (call-process-shell-command
                (format "cat | ssh %s maildrop %s"
                        my-mailfilter-server
                        my-mailfilter-test-file)
                my-mailfilter-testmsg
                t
                nil)))

      (unless (= res 0)
        (error (buffer-string))))))
      

;; test different w3m map
(require 'w3m)
(define-key w3m-minor-mode-map (kbd "<RET>") 'w3m-view-url-with-external-browser)
