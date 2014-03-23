(require 'cl)
;; Next steps: 
;;
;;  - handle the case where the custom-file is not present
;;
;;  - warn or error when variables are customized that are not
;;    mentioned in the required variables? Make this optional? (and
;;    exclude the `check-customize-ignore-list' variable)
;;
;;  - clean up obsolete code

(defcustom check-customize-ignore-list nil 
  "A list of variables that should be ignored by check-customize.")

(defun check-customize (variables custom-file)
  "Check if the variables in `variables' were customized in file
  `custom-file'. Checks for specific variables can be disabled by
  customizing `check-customize-ignore-list'"
  (let* ((customs (check-customize-read-customs-from-file custom-file))
	(missing (check-customize-get-missing
		  variables
		  customs))
	(additional (check-customize-get-additional
		     (append variables '(check-customize-ignore-list))
		     customs)))
    (if (not (and (null missing) (null additional)))
	(let ((buf (check-customize-create-buffer))
	      (win (split-window)))
	  (with-current-buffer buf
	    (check-customize-write-missing-forms missing)
	    (check-customize-write-additional-values additional))
	  (set-window-buffer win buf))
      (message "check-customize: all required customizations are present"))))

(defun check-customize-create-buffer ()
  "Create a buffer to put the missing customizations into."
  (let ((buf (generate-new-buffer "*Missing customizations")))
    (with-current-buffer buf
      (lisp-mode)
      (insert ";; Press C-c C-c to close\n\n")
      (local-set-key (kbd "C-c C-c") 'delete-window))
    buf))

(defun check-customize-customize-form (var)
  "Create a form for customizing `var'"
  (format "(customize-variable-other-window '%s)" var))

(defun check-customize-write-missing-forms (missing)
  "Write the `missing' forms into the current buffer"
  (dolist (v missing)
    (insert (check-customize-customize-form v))
    (insert "\n")))

(defun check-customize-write-additional-values (additional)
  (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
  (insert ";; additional custumizations that I don't know about:\n;;\n")
  (dolist (v additional)
    (insert (format ";; %s\n" v))))

(defun check-customize-get-missing (variables file-content)
  "Return the variables that are not customized by
  `file-content' (a list with customizations, like the
  `custom-set-variables' variable)."
    (let ((to-check (remove-if () variables))
	  (customs (mapcar 'car file-content)))
    (remove-if (lambda (x) (or (member x check-customize-ignore-list)
			       (member x customs))) to-check)))

(defun check-customize-get-additional (variables file-content)
  "Return the variables that are customized but not listed as needed. The result is an alist of variables and values."
  (remove-if (lambda (x) (member (car x) variables)) file-content))

(defun check-customize-read-customs-from-file (custom-file-name)
  "Read the variables set in a custom file (from a file)."
  (with-temp-buffer
    (insert-file-literally custom-file-name)
    (beginning-of-buffer)
    (check-customize-read-customs (current-buffer))))

(defun check-customize-read-customs (custom-buffer)
  "Read the variables set in a custom file (from a buffer)."
  (cl-assert (bufferp custom-buffer) t "Expecting a buffer!")
  (mapcar 'cadr (cl-subseq (read custom-buffer) 1)))

;; Obsolete ;;;;;;;;;;;;;;;;;;;;;
(defun check-customize-prompt (var)
  "Ask if variable VAR should be customized now, ignored now or
add to ignored variables. The answer is returned as one of the
symbols: `ignore', `custom', or `add'"
  (check-customize-read-string-validate
   (format "Variable %s is not customized! (i)gnore once, (a)dd to ignored, (c)ustomize now"
	   var)
   (lambda (answer) 
     (cond
      ((string= answer "i") 'ignore)
      ((string= answer "a") 'add)
      ((string= answer "c") 'custom)
      (t (error "Invalid answer: %s" answer))))
   nil
   nil
   "i"))

;; Todo: move this to a general utils module
(defun check-customize-read-string-validate (prompt parser &optional initial-input history default-value inherit-input-method)
  "Read a string from the minibuffer, apply the function PARSER
  to the answer and return the result. If PARSER signals an error, the user is promped again."
  (let ((answer nil)
	(should-ask t))
    (while should-ask
      (condition-case err
	  (setq answer (apply parser (read-string prompt
						  initial-input
						  history
						  default-value
						  inherit-input-method)))
	(error
	 (message (cdr err))
	 (setq should-ask t))))
    answer))

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

(ert-deftest test-additional ()
  "some additional variables"
  (let ((vars '(bbdb-file-remote
		some-var-1
		some-var-2
		is-ignored))
	(customs '((bbdb-file-remote . "some/file/name")
		   (bbdb-file-remote-2 . "some/file/name2")
		   (is-ignored . "ignore-me")
		   (some-var-1 . "var1")
		   (some-var-3 . "var3"))))
    (should (equal (check-customize-get-additional vars customs)
		   '((bbdb-file-remote-2 . "some/file/name2")
		     (some-var-3 . "var3"))))))

(ert-deftest test-parse-custom-file ()
  "reading the custom file works as expected"
  (should (equal
	   (with-temp-buffer
	     (insert (check-customize-test-file))
	     (beginning-of-buffer)
	     (check-customize-read-customs (current-buffer)))
	   (check-customize-test-custom-content))))

(ert-deftest test-variable-form ()
  "customize form for a missing variable"
  (should (equal (check-customize-customize-form 'bbdb-file-remote)
		 "(customize-variable-other-window 'bbdb-file-remote)")))
(ert-deftest test-variable-missing-forms ()
  "emit customize forms for a bunch of missing variables"
  (let ((missing '(bbdb-file-remote
		   is-missing))
	(output (concat "(customize-variable-other-window 'bbdb-file-remote)\n"
			"(customize-variable-other-window 'is-missing)\n")))
    (should (equal (with-temp-buffer (check-customize-write-missing-forms missing)
				     (buffer-string))
		   output))))

(provide 'check-customize)
