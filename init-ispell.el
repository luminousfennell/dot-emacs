(defvar my-langs '("german" "en_US" "hello"))
(defvar my-lang-ring)

(defvar my-flyspell-buffer-p nil)
(make-variable-buffer-local 'my-flyspell-buffer-p)

(defun my-cycle-ispell-new-lang-ring (langs)
  (let ((ring (make-ring (length langs))))
    (dolist (elem langs ring) (ring-insert ring elem))))

(defun my-cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref my-lang-ring -1)))
    (ispell-change-dictionary lang)
    (ring-insert my-lang-ring lang)
    (when my-flyspell-buffer-p (flyspell-buffer))))

(global-set-key [f6] 'cycle-ispell-languages)


;; initialize
(setq my-langs (with-temp-buffer
                 (let ((langs nil))
                   (dolist (l my-langs langs)
                     (condition-case nil (progn (ispell-change-dictionary l)
                                                (setq langs (cons l langs)))
                       (error (message (format "Dictionary `%s' not found." l))))))))
(setq my-lang-ring (my-cycle-ispell-new-lang-ring my-langs))

;; setup flyspell hook
(add-hook 'flyspell-mode-hook (setq my-flyspell-buffer-p t))


