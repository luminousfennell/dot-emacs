;; Define bbdb mail aliases for mail-abbrevs-mode
(require 'bbdb "~/.emacs.d/bbdb/lisp/bbdb.el")
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-message) 
;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases) 
 (setq bbdb-complete-mail-allow-cycling t)
