;; Functions that make it easier to use dired as a file manager
;; replacement.
;; 
;; Features so far:
;;  - dired-fm-open open file with external application, except when it's a directory
;;  - dired-fm-open-terminal open a terminal emulator in the current directory


;; TODO: make the default optional. When nil, nothing is presented by default
(defcustom dired-fm-default-launcher "xdg-open"
  "The program used for the default suggestion of `dired-fm-open'"
  )

(defcustom dired-fm-terminal-command "urxvt"
  "The terminal program used by `dired-fm-open-terminal'"
  )

(defcustom dired-fm-bash-command "/bin/sh"
  "Command to invoke bash. Bash is used for spawning other
  programs, e.g. in dired-fm-open-terminal.")

(defcustom dired-fm-query-mime-type-command "xdg-mime query filetype %s"
  "Command to query mime types. Use `%s' as a placeholder for the file name.")

(defcustom dired-fm-set-default-application-command "xdg-mime default %s %%s"
  "Command to set default applications. Use `%s' as a placeholder
  for the application and `%%s' as a placeholder for the
  mime-type.")

(defun dired-fm-spawn (command args)
  "Spawn COMMAND with the list of arguments ARGS."
  ;; This trick for spawning applications is shamelessly stolen from
  ;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
  (let ((process-connection-type nil))
    (start-process "" nil
		   dired-fm-bash-command
		   "-c" (concat command " " (mapconcat 'shell-quote-argument args " "))
		   "&")))

(defun dired-fm-open-terminal ()
  "Open a terminal from the current directory. The value of
`dired-fm-terminal-command' is used to start the terminal. "
  (interactive)
  (dired-fm-spawn dired-fm-terminal-command nil)
  )

(defun dired-fm-open (&optional arg)
  "Spawn an external application to open currently selected
file. With prefix arg, just do `dired-find-file'."
  (interactive "P")
  (cond
   (arg (dired-find-file))
   (t (let ((file (dired-get-file-for-visit)))
	;; TODO: allow customizable escape hatches
	;; - with a regex
	;; - or with a function
        (if (file-directory-p file) (dired-find-file)
          (let* ((file-list `(,file))
		 (program
		  (let ((dired-guess-shell-alist-user ;; set the default prompt
			 `((".*"  dired-fm-default-launcher))))
		    (dired-read-shell-command "Open %s with: " nil file-list)));; TODO ask the user
		(cmd 
		 ))
	    (message "Spawning `%s %s'" program (mapconcat 'identity file-list " "))
	    ;; TODO: allow to specify * for the file name, just like dired-shell-command
            (dired-fm-spawn program file-list)))))))
  
(defun dired-fm-query-mime-type (file-name)
  (car
   (split-string
    (shell-command-to-string (format dired-fm-query-mime-type-command
				     (shell-quote-argument file-name)))
    ";")))

(defun dired-fm-show-mime-type ()
  (interactive)
  (message (dired-fm-query-mime-type (dired-get-file-for-visit))))

;; TODO: make an interactive command for this
(defun dired-fm-set-default-application ()
  "Set default application for selected file, using `diref-fm-set-default-application-command'."
  (interactive)
  (let* ((file-name (dired-get-file-for-visit))
	 (app (read-string (format "Default application for `%s': " (dired-fm-query-mime-type file-name)))))
    ;; TODO: look for desktop files and provide them as completion
    ;; TODO: validate existance of desktop file
    (when app
      (call-process-shell-command
       (format (format dired-fm-set-default-application-command (shell-quote-argument app))
	       (dired-fm-query-mime-type file-name))))))


(provide 'dired-fm)
