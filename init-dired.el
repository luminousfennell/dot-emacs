(require 'dired-details+)

(setq
 dired-bind-jump nil
 dired-details-hidden-string ""
 ;; magic copy/rename targets (really cool)
 dired-dwim-target t
 ;; directories should be listed first
 dired-listing-switches "-lah --group-directories-first"
 dired-omit-files "^\\.?#\\|^\\.$\\|^\\.[^\\.].*$"
 dired-omit-verbose nil
 dired-recursive-copies 'always
 dired-recursive-deletes 'always
 )

;; hook
(add-hook 'dired-mode-hook
	  (lambda ()
	    (auto-revert-mode t)
	    ;; keybindings
	    (evil-define-key 'normal dired-mode-map ")" 'dired-details-toggle)))


;;dired-x for hidden view
(add-hook 'dired-load-hook (lambda ()
                             (load "dired-x")))
(add-hook 'dired-mode-hook (lambda ()
                             (load "dired-x" nil t)
                             (dired-omit-mode 1)
                             (auto-revert-mode t)))
