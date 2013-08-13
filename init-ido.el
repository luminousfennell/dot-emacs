(require 'ido)
(setq 
 ido-auto-merge-work-directories-length -1
 ido-default-buffer-method (quote selected-window)
 ido-default-file-method (quote selected-window)
 ido-enable-flex-matching t
 ido-show-dot-for-dired t)

(ido-mode t)
