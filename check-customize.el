(require 'cl)
;; Next steps: tests for the list of missing variables generation:
;;  - example custom file
;;  - basic found and not found stuff
;;  - also with ignored variables
;;  - handle the case where the custom-file is not present

(defcustom check-customize-ignore-list nil 
  "A list of variables that should be ignored by check-customize.")

(defun check-customize (variables custom-file)
  "Check if the variables in `variables' were customized in file
  `custom-file'. Checks for specific variables can be disabled by
  customizing `check-customize-ignore-list'"
    (dolist (v (check-customize-get-missing variables (check-customize-read-customs custom-file)))
      (message (format "Variable %s is not customized!" v))))

(defun check-customize-get-missing (variables file-content)
  "Return the variables that are not customized by
  `file-content' (a list with customizations)."

    (let ((to-check (remove-if () variables))
	  (customs (mapcar 'caadr file-content)))
    (remove-if (lambda (x) (or (member x check-customize-ignore-list)
			       (member x customs))) to-check)))

(defun check-customize-read-customs (custom-file)
  "Read the variables set in a custom file."
  (let* ((b (find-file-noselect custom-file))
	 (cs (cl-subseq (read b) 1)))
    (kill-buffer b)
    cs))

(provide 'check-customize)
