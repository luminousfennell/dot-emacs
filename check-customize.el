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
    (dolist (v (check-customize-get-missing
		variables
		(check-customize-read-customs-from-file custom-file)))
      (message (format "Variable %s is not customized!" v))))

(defun check-customize-get-missing (variables file-content)
  "Return the variables that are not customized by
  `file-content' (a list with customizations)."

    (let ((to-check (remove-if () variables))
	  (customs (mapcar 'car file-content)))
    (remove-if (lambda (x) (or (member x check-customize-ignore-list)
			       (member x customs))) to-check)))

(defun check-customize-read-customs-from-file (custom-file-name)
  "Read the variables set in a custom file (from a file)."
  (with-temp-buffer
    (insert-file-literally custom-file-name)
    (beginning-of-buffer)
    (check-customize-read-customs (current-buffer))))

(defun check-customize-read-customs (custom-file)
  "Read the variables set in a custom file (from a buffer)."
  (cl-assert (bufferp custom-file) t "Expecting a buffer!")
  (mapcar 'cadr (cl-subseq (read custom-file) 1)))


;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ert)
(defun check-customize-test-file ()
  ""
  "(custom-set-variables
   '(bbdb-file-remote \"~/org/bbdb\")
   '(org-mobile-directory \"~/media/PHONE/org\")
   '(send-mail-function (quote sendmail-send-it))
   '(text-mode-hook (quote (turn-on-flyspell text-mode-hook-identify))))
  (custom-set-faces
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
   )
 ")
;; 
   ;; 
(defun check-customize-test-custom-content ()
   '(
     (bbdb-file-remote "~/org/bbdb")
     (org-mobile-directory "~/media/PHONE/org")
     (send-mail-function (quote sendmail-send-it))
     (text-mode-hook (quote (turn-on-flyspell text-mode-hook-identify)))))

(ert-deftest test-missing-all ()
  "all customization is required, none is defined"
  (should (equal
	   (check-customize-get-missing
	   '(bbdb-file-remote
	     org-mobile-directory
	     send-mail-function
	     text-mode-hook) 
	    '())
	   '(bbdb-file-remote
	     org-mobile-directory
	     send-mail-function
	     text-mode-hook))))

(ert-deftest test-none-required ()
  "No error when we require no variables"
  (should (equal
	   (check-customize-get-missing
	    '()
	    (check-customize-test-custom-content))
	   '())))

(ert-deftest test-match-some ()
  "match some of the variables, others not"
  (should (equal
	   (check-customize-get-missing
	    '(bbdb-file-remote
	      is-missing)
	    (check-customize-test-custom-content))
	   '(is-missing))))

(ert-deftest test-match-some-more ()
  "match some more of the variables, others not"
  (should (equal
	   (check-customize-get-missing
	    '(bbdb-file-remote
	      is-missing
	      is-missing-too)
	    (check-customize-test-custom-content))
	   '(is-missing is-missing-too))))

(ert-deftest test-match-middle ()
  "match some more of the variables, others not"
  (should (equal
	   (check-customize-get-missing
	    '(is-missing
	      bbdb-file-remote
	      is-missing-too)
	    (check-customize-test-custom-content))
	   '(is-missing is-missing-too))))

(ert-deftest test-ignore-some ()
  "some missing vars are ignored"
  (should (equal
	   (let ((check-customize-ignore-list '(is-ignored)))
	     (check-customize-get-missing
	    '(bbdb-file-remote
	      is-missing
	      is-ignored)
	    (check-customize-test-custom-content)))
	   '(is-missing))))

(ert-deftest test-parse-custom-file ()
  "reading the custom file works as expected"
  (should (equal
	   (with-temp-buffer
	     (insert (check-customize-test-file))
	     (beginning-of-buffer)
	     (check-customize-read-customs (current-buffer)))
	   (check-customize-test-custom-content))))


(provide 'check-customize)
