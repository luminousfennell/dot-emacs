(provide 'my-mbsync)

(defun my-mbsync ()
  (interactive)
  (async-shell-command "mbsync --all"))
