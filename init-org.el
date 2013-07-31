(require 'org-install)

;; global keybindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

;; TODO: find a solution to sanely use spell checking in org mode
(add-hook 'org-mode-hook (lambda () (flyspell-mode 0)))


;; Automatically close a todo item when all children are finished
;; snippet taken from the org manual
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
