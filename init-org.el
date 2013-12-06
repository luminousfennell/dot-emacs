(require 'org-install)

;; options
(setq
 org-M-RET-may-split-line (quote ((default)))
 org-checkbox-hierarchical-statistics nil
 org-enforce-todo-checkbox-dependencies t
 org-enforce-todo-dependencies t
 org-hierarchical-todo-statistics nil
 org-mobile-force-id-on-agenda-items nil
 org-pretty-entities nil
 org-startup-indented t
 org-yank-adjusted-subtrees t)


;; agenda files... set in the load hook in order to access org-directory
(add-hook 'org-load-hook
	  (lambda ()
	    (setq org-agenda-files
		  (concat (file-name-as-directory org-directory)
			  "agenda-files.lst"))))

;; global keybindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

;; TODO: find a solution to sanely use spell checking in org mode
(add-hook 'org-mode-hook (lambda () (flyspell-mode -1)))


;; Automatically close a todo item when all children are finished
;; snippet taken from the org manual
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bugfixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BUG: the workaround for what == addheading should not call
;; `org-insert-heading-respect-content' with double prefix arg, I
;; think; with the double prefix arg, the heading is inserted at the
;; under the last heading of the *parent* subtree (which is not in
;; general the one we want to insert under)
(require 'org-mobile)
(defun org-mobile-edit (what old new)
  "Edit item WHAT in the current entry by replacing OLD with NEW.
WHAT can be \"heading\", \"todo\", \"tags\", \"priority\", or \"body\".
The edit only takes place if the current value is equal (except for
white space) the OLD.  If this is so, OLD will be replace by NEW
and the command will return t.  If something goes wrong, a string will
be returned that indicates what went wrong."
  (let (current old1 new1 level)
    (if (stringp what) (setq what (intern what)))

    (cond

     ((memq what '(todo todostate))
      (setq current (org-get-todo-state))
      (cond
       ((equal new "DONEARCHIVE")
	(org-todo 'done)
	(org-archive-subtree-default))
       ((equal new current) t) ; nothing needs to be done
       ((or (equal current old)
	    (eq org-mobile-force-mobile-change t)
	    (memq 'todo org-mobile-force-mobile-change))
	(org-todo (or new 'none)) t)
       (t (error "State before change was expected as \"%s\", but is \"%s\""
		 old current))))

     ((eq what 'tags)
      (setq current (org-get-tags)
	    new1 (and new (org-split-string new ":+"))
	    old1 (and old (org-split-string old ":+")))
      (cond
       ((org-mobile-tags-same-p current new1) t) ; no change needed
       ((or (org-mobile-tags-same-p current old1)
	    (eq org-mobile-force-mobile-change t)
	    (memq 'tags org-mobile-force-mobile-change))
	(org-set-tags-to new1) t)
       (t (error "Tags before change were expected as \"%s\", but are \"%s\""
		 (or old "") (or current "")))))

     ((eq what 'priority)
      (when (looking-at org-complex-heading-regexp)
	(setq current (and (match-end 3) (substring (match-string 3) 2 3)))
	(cond
	 ((equal current new) t) ; no action required
	 ((or (equal current old)
	      (eq org-mobile-force-mobile-change t)
	      (memq 'tags org-mobile-force-mobile-change))
	  (org-priority (and new (string-to-char new))))
	 (t (error "Priority was expected to be %s, but is %s"
		   old current)))))

     ((eq what 'heading)
      (when (looking-at org-complex-heading-regexp)
	(setq current (match-string 4))
	(cond
	 ((equal current new) t) ; no action required
	 ((or (equal current old)
	      (eq org-mobile-force-mobile-change t)
	      (memq 'heading org-mobile-force-mobile-change))
	  (goto-char (match-beginning 4))
	  (insert new)
	  (delete-region (point) (+ (point) (length current)))
	  (org-set-tags nil 'align))
	 (t (error "Heading changed in MobileOrg and on the computer")))))

     ((eq what 'addheading)
      (if (org-at-heading-p) ; if false we are in top-level of file
	  (progn
	    ;; Workaround a `org-insert-heading-respect-content' bug
	    ;; which prevents correct insertion when point is invisible
	    (org-show-subtree)
	    (end-of-line 1)
	    ;; BUG: see above
	    (org-insert-heading-respect-content nil t)
	    (org-demote))
	(beginning-of-line)
	(insert "* "))
      (insert new))

     ((eq what 'refile)
      (org-copy-subtree)
      (org-with-point-at (org-mobile-locate-entry new)
	(if (org-at-heading-p) ; if false we are in top-level of file
	    (progn
	      (setq level (org-get-valid-level (funcall outline-level) 1))
	      (org-end-of-subtree t t)
	      (org-paste-subtree level))
	  (org-paste-subtree 1)))
      (org-cut-subtree))

     ((eq what 'delete)
      (org-cut-subtree))

     ((eq what 'archive)
      (org-archive-subtree))

     ((eq what 'archive-sibling)
      (org-archive-to-archive-sibling))

     ((eq what 'body)
      (setq current (buffer-substring (min (1+ (point-at-eol)) (point-max))
				      (save-excursion (outline-next-heading)
						      (point))))
      (if (not (string-match "\\S-" current)) (setq current nil))
      (cond
       ((org-mobile-bodies-same-p current new) t) ; no action necessary
       ((or (org-mobile-bodies-same-p current old)
	    (eq org-mobile-force-mobile-change t)
	    (memq 'body org-mobile-force-mobile-change))
	(save-excursion
	  (end-of-line 1)
	  (insert "\n" new)
	  (or (bolp) (insert "\n"))
	  (delete-region (point) (progn (org-back-to-heading t)
					(outline-next-heading)
					(point))))
	t)
       (t (error "Body was changed in MobileOrg and on the computer")))))))



;; BUG: the olp links are (sometimes?) inserted with a space at the
;; end. So we trim them here. But perhaps that's a bug in android
;; org-mobile instead.
(defun org-mobile-locate-entry (link)
  (if (string-match "\\`id:\\(.*\\)$" link)
      (org-id-find (match-string 1 link) 'marker)
    ;; BUG: see above; the following is just a workaround
    (setq link (org-trim link))
    (if (not (string-match "\\`olp:\\(.*?\\):\\(.*\\)$" link))
					; not found with path, but maybe it is to be inserted
					; in top level of the file?
	(if (not (string-match "\\`olp:\\(.*?\\)$" link))
	    nil
	  (let ((file (match-string 1 link)))
	    (setq file (org-link-unescape file))
	    (setq file (expand-file-name file org-directory))
	    (save-excursion
	      (find-file file)
	      (goto-char (point-max))
	      (newline)
	      (goto-char (point-max))
	      (point-marker))))
      (let ((file (match-string 1 link))
	    (path (match-string 2 link)))
	(setq file (org-link-unescape file))
	(setq file (expand-file-name file org-directory))
	(setq path (mapcar 'org-link-unescape
			   (org-split-string path "/")))
	(org-find-olp (cons file path))))))
