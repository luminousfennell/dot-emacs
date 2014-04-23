;; list variables that should be customized and call
;; `check-customized' with them.

(require 'check-customize "~/.emacs.d/check-customize.el")

(defvar check-customize-list
  '(bbdb-file-remote
    gnus-posting-styles
    message-send-mail-function
    org-directory
    org-mobile-directory
    user-full-name
    user-mail-address)
  )

(check-customize check-customize-list custom-file)
