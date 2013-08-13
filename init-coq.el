;; customization
(setq
 coq-compile-before-require t
 coq-one-command-per-line nil
 coq-script-indent nil
 coq-unicode-tokens-enable nil
 proof-auto-action-when-deactivating-scripting (quote retract)
 proof-electric-terminator-enable nil
 proof-follow-mode (quote ignore)
 proof-imenu-enable nil
 proof-script-fly-past-comments t
 proof-strict-read-only t
 proof-three-window-enable t)

;; set the search command (customization does not work somehow)
(add-hook 'coq-mode-hook
	  (lambda ()
	    (setq proof-find-theorems-command "SearchAbout %s")))
