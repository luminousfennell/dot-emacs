;; use the newer org-mode
(add-to-list 'load-path "~/.emacs.d/org-7.9.1/lisp")
(require 'org-install "~/.emacs.d/org-7.9.1/lisp/org-install" t)

;; global keybindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

;; use abbrevs
(add-hook 'org-mode-hook 'abbrev-mode)

(add-hook 'org-mode-hook (lambda () (flyspell-mode 0)))

;; try to include agenda items into the diary
(add-hook 'diary-display-hook 'diary-fancy-display-mode)
(setq org-agenda-diary-file "~/diary.org"
      diary-file "~/org/diary")


;; and mark today in the calendar
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;; a function to display a calendar and todo lists
(defun my-diary()
  (interactive)
  (org-agenda-list)
  (calendar))

;; Automatically close a todo item when all children are finished
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;; (setq org-insert-heading-hook 'org-add-statistics-to-heading)
(setq org-insert-heading-hook nil)
(defun org-add-statistics-to-heading ()
  (save-excursion 
    (when (org-up-heading-safe)
      (unless (string-match ".*\\[.*\\][[:space:]]*$" 
                            (buffer-substring (line-beginning-position) 
                                              (line-end-position)))
        (goto-char (line-end-position))
        (insert "[%]")
        (org-update-statistics-cookies nil)))
    (org-get-heading)))

;; (set-face-attribute 'org-todo-face `(:foreground  ,zenburn-red-3
;;                                                   :weight bold))
;; (set-face-attribute 'org-todo nil 
;;                     :foreground  zenburn-red-3
;;                     :weight 'bold)
;; (set-face-attribute 'org-done nil 
;;                     :foreground  zenburn-green-1
;;                     :weight 'bold)
