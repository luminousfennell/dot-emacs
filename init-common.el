;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization and bootstrapping 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; init package management
(require 'package)
(setq 
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		    ("marmalade" . "http://marmalade-repo.org/packages/")
		    ("melpa" . "http://melpa.milkbox.net/packages/")
		    ("org" . "http://orgmode.org/elpa/")
		    )
 package-enable-at-startup nil)
(package-initialize)

;; initialize customization
(setq custom-file "~/.emacs.d/init-custom.el")
(load custom-file)
(load-file "~/.emacs.d/perform-customization-check.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core editor settings 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq
 auto-revert-verbose nil
 column-number-mode t
 display-time-mode t
 frame-title-format "%b %f"
 global-auto-revert-mode t
 global-auto-revert-non-file-buffers t
 indent-tabs-mode nil
 inhibit-startup-screen t
 make-backup-files nil
 ring-bell-function 'ignore
 show-paren-mode t
 vc-follow-symlinks nil
 x-select-enable-clipboard t
 )

;; bindings
(defun my-bind-window-movement ()
  (global-set-key "\C-x\C-j" 'windmove-left)   
  (global-set-key "\C-x\C-l" 'windmove-right)       
  (global-set-key "\C-x\C-i" 'windmove-up)          
  (global-set-key "\C-x\C-k" 'windmove-down))
(my-bind-window-movement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package customization 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

(use-package ediff
  :init (setq ediff-window-setup-function (quote ediff-setup-windows-plain)))

;; evil mode
(use-package evil
  :ensure evil
  :init (progn
	  (evil-mode t)
	  (setq
	   evil-default-cursor '(t ignore)
	   evil-want-fine-undo t
	   )
	  (load-file "~/.emacs.d/perform-evil-mode-bindings.el")))
(use-package surround
  :ensure surround
  :init (global-surround-mode t))

;; custom common package initialization
;; (load-file "~/.emacs.d/init-evil.el")
(load-file "~/.emacs.d/init-surround.el")
(load-file "~/.emacs.d/init-tramp.el")
(load-file "~/.emacs.d/init-ido.el")


