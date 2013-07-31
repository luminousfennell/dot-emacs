;; misc settings
(setq
 auto-revert-verbose nil
 column-number-mode t
 display-time-mode t
 ediff-window-setup-function (quote ediff-setup-windows-plain)
 indent-tabs-mode nil
 inhibit-startup-screen t
 frame-title-format "%b %f"
 global-auto-revert-mode t
 global-auto-revert-non-file-buffers t
 make-backup-files nil
 ring-bell-function 'ignore
 recentf-mode t
 show-paren-mode t
 vc-follow-symlinks nil
 x-select-enable-clipboard t
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spellchecking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefer M-TAB of the other modes
;; (add-to-list 'flyspell-mode-hook
(set 'flyspell-mode-hook
     (lambda ()
       (define-key flyspell-mode-map (kbd "M-TAB") nil)
       (define-key flyspell-mode-map (kbd "C-<tab>") 
         'flyspell-auto-correct-word)))


;; common keybindings
(defun my-bind-window-movement ()
  (global-set-key "\C-x\C-j" 'windmove-left)   
  (global-set-key "\C-x\C-l" 'windmove-right)       
  (global-set-key "\C-x\C-i" 'windmove-up)          
  (global-set-key "\C-x\C-k" 'windmove-down))
(my-bind-window-movement)

;; use ibuffer by default
 (global-set-key "\C-x\C-b" 'ibuffer)

;; wrap the shell command to be sure it's buffer is recorded
;; (otherwise it's anoying to use emacsclient from the shell)
(defun sh ()
  (interactive)
  (shell)
  (switch-to-buffer (current-buffer)))

;; now load additional init files if they exist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first, add the config directory for convenience
(add-to-list 'load-path "~/.emacs.d")

 ;; themeing
(if (boundp 'custom-theme-load-path) 
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (set 'custom-theme-load-path "~/.emacs.d/themes")
  (condition-case err
      (load-file  "~/.emacs.d/themes/zenburn-theme.el")
    (error nil)))


;; deal with customize
(setq custom-file "~/.emacs.d/init-custom.el")
(load custom-file)

;; now we initialize the elpa-packages
(require 'package)
(setq 
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		    ("marmalade" . "http://marmalade-repo.org/packages/")
		    ("melpa" . "http://melpa.milkbox.net/packages/")
		    ("org" . "http://orgmode.org/elpa/"))
 package-enable-at-startup nil)
(package-initialize)

;; custom common package initialization
(load-file "~/.emacs.d/init-evil.el")
