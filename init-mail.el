;; General config
(setq
 gnus-propagate-marks t
 gnus-read-newsrc-file nil
 gnus-save-newsrc-file nil
 gnus-use-dribble-file nil
 mail-specify-envelope-from t
 mail-user-agent 'gnus-user-agent
 message-citation-line-format "On %a, %b %d %Y at %R %z, %N wrote:
"
 message-citation-line-function 'message-insert-formatted-citation-line
 message-confirm-send t
 message-sendmail-f-is-evil t
 mm-text-html-renderer 'w3m)

;; BBDB settings
(require 'bbdb)
(add-hook 'mail-mode-hook 'mail-abbrevs-mode)

;; enable imap searching for gnus
(require 'nnir)

;; test different w3m map
(require 'w3m)
(define-key w3m-minor-mode-map (kbd "<RET>") 'w3m-view-url-with-external-browser)
