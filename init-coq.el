;; customization
(setq
 coq-compile-before-require t
 coq-one-command-per-line nil
 coq-script-indent nil
 coq-unicode-tokens-enable nil
 coq-user-tactics-db (quote (("introv" "introv" "introv" t "introv") ("inverts" "inverts" "inverts" t "inverts") ("cases" "cases" "cases" t "cases") ("cases_if" "cases_if" "cases_if" t "cases?_if") ("splits" "splits" "splits" t "splits") ("branch" "branch" "branch" t "branch") ("exists" "exists" "exists" t "exists") ("asserts_rewrite" "asserts_rewrite" "asserts_rewrite" t "asserts_rewrite") ("cuts_rewrite" "cuts_rewrite" "cuts_rewrite" t "cuts_rewrite") ("substs" "substs" "substs" t "substs") ("fequals" "fequals" "fequals" t "fequals") ("applys_eq" "applys_eq" "applys_eq" t "applys_eq") ("unfolds" "unfolds" "unfolds" t "unfolds") ("false" "false" "false" t "false") ("tryfalse" "tryfalse" "tryfalse" t "tryfalse") ("gen" "gen" "gen" t "gen") ("gen_eq" "gen_eq" "gen_eq" t "gen_eq") ("skip" "skip" "skip" t "skip") ("skip_rewrite" "skip_rewrite" "skip_rewrite" t "skip_rewrite") ("skip_goal" "skip_goal" "skip_goal" t "skip_goal") ("sort" "sort" "sort" t "sort") ("lets" "lets" "lets" t "lets") ("forwards" "forwards" "forwards" t "forwards") ("applys" "applys" "applys" t "applys") ("specializes" "specializes" "specializes" t "specializes") ("simpls" "simpls" "simpls" t "simpls")))
)
;; set the search command (customization does not work somehow)
(add-hook 'coq-mode-hook (lambda ()
                           (setq proof-find-theorems-command "SearchAbout %s")
                           ;; (auto-complete-mode t)
                           ))

;; change C-n to the same thing as electric .
(defun my-pg-assert-until-point ()
  (interactive)  
  (if (<= (point) (proof-queue-or-locked-end))
      (proof-assert-next-command-interactive)
    (let ((no-more-cmd-in-line
           (save-excursion
             (skip-chars-backward "\n\t ")
             (<= (point) (proof-queue-or-locked-end)))))
      (when no-more-cmd-in-line
        (skip-chars-forward "\n\t ")))
    (proof-assert-until-point-interactive)))

