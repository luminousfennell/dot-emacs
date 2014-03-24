;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization and bootstrapping 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; personal extensions
(add-to-list 'load-path "~/.emacs.d/lib")

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
 auto-save-list-file-prefix "~/.cache/emacs.d/auto-save-list/.saves-"
 column-number-mode t
 display-time-mode t
 frame-title-format "%b %f"
 global-auto-revert-mode t
 global-auto-revert-non-file-buffers t
 indent-tabs-mode nil
 inhibit-startup-screen t
 make-backup-files nil
 ps-print-header nil
 ring-bell-function 'ignore
 vc-follow-symlinks nil
 x-select-enable-clipboard t
 )
;; switches
(show-paren-mode t)
;; bindings
(defun my-bind-window-movement ()
  (global-set-key "\C-x\C-j" 'windmove-left)   
  (global-set-key "\C-x\C-l" 'windmove-right)       
  (global-set-key "\C-x\C-i" 'windmove-up)          
  (global-set-key "\C-x\C-k" 'windmove-down))
(my-bind-window-movement)
;; hooks
(add-hook 'text-mode-hook
          'turn-on-flyspell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package customization 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;; path initialization
;; agda
(condition-case err
    (load-file (shell-command-to-string "agda-mode locate"))
  (error (message "Warning: %s" (error-message-string err))))
;; coq
;; TODO: why isn't nix handling this?
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/ProofGeneral/generic")

;; begin config
(use-package vc
  :init (progn
          (setq
           vc-follow-symlinks nil
           ;; TODO: remove
           ;; vc-annotate-background "#2B2B2B"
           ;; vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3")))
           ;; vc-annotate-very-old-color "#DC8CC3"
           )))

(use-package ediff
  :init (setq ediff-window-setup-function (quote ediff-setup-windows-plain)))

(use-package paredit
  :ensure paredit
  :init (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t))))

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
;; end evil mode

(use-package tramp
  :init (add-to-list 'tramp-remote-path "/var/run/current-system/sw/bin"))

(use-package ido
  :init (progn
	  ;; vars
	  (setq 
	   ido-auto-merge-work-directories-length -1
	   ido-default-buffer-method (quote selected-window)
	   ido-default-file-method (quote selected-window)
	   ido-enable-flex-matching t
	   ido-show-dot-for-dired t)
	  ;; switches
	  (ido-mode t))) 

(use-package auctex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  ;; TODO: why is `config' not working
  (add-hook 'LaTeX-mode-hook
	    ;; dependencies
	    '(lambda ()
	       (use-package my-LaTeX-environment)
	       ;; config
	       (setq
		reftex-plug-into-AUCTeX t
		TeX-auto-save t
                TeX-PDF-mode t
		TeX-parse-self t
		TeX-source-correlate-mode t)
	       ;; keybindings
	       (define-key LaTeX-mode-map (kbd "M-q") 'noop)
	       (define-key LaTeX-mode-map (kbd "C-c C-e") 'my-LaTeX-environment)
	       (define-key LaTeX-mode-map (kbd "SPC") 'my-LaTeX-break-after-sentence-or-space)
	       (push '(?% . ("$" . "$")) surround-pairs-alist)
	       ;; switches
	       (turn-on-reftex)
	       (outline-minor-mode t)
	       (visual-line-mode t)
	       (set (make-local-variable 'visual-line-fringe-indicators) t))))

(use-package org
  :ensure org
  :mode ("\\.org\\'" . org-mode)
  :init
  (progn
    (setq
     org-highest-priority ?A
     org-default-priority ?M
     org-lowest-priority  ?Z

     org-M-RET-may-split-line '((default))
     org-checkbox-hierarchical-statistics nil
     org-enforce-todo-checkbox-dependencies t
     org-enforce-todo-dependencies t
     org-hierarchical-todo-statistics nil
     org-mobile-force-id-on-agenda-items nil
     org-pretty-entities nil
     org-startup-indented t
     org-yank-adjusted-subtrees t)
    ;; keybindings
    ;; TODO: why is :bind not working?
    (bind-key "\C-ca"  'org-agenda)
    (bind-key "\C-cc"  'org-capture)
    (add-to-list 'evil-motion-state-modes 'org-agenda-mode)
    (evil-define-key 'motion org-agenda-mode-map
      "g" (lambda () (interactive) (org-agenda-redo t))
      "+" 'org-agenda-priority-up
      "-" 'org-agenda-priority-down)
    ;; hooks
    (add-hook 'org-load-hook
    	      (lambda ()
    		(use-package my-org-utils)
    		(use-package my-org-fixes)
    		;; set the following in the load hook in order to access org-directory
    		(setq org-agenda-files
    		      (concat (file-name-as-directory org-directory)
    			      "agenda-files.lst")
                      org-default-notes-file
                      (concat (file-name-as-directory org-directory)
    			      "notes.org"))))
    (add-hook 'org-mode-hook
    	      (lambda ()
    		(flyspell-mode -1)))
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
    (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
    ))

(use-package dired
  :init
  (progn
    (use-package dired-details+
      :ensure dired-details+)
    (setq
     dired-bind-jump nil
     dired-details-hidden-string ""
     ;; magic copy/rename targets (really cool)
     dired-dwim-target t
     ;; directories should be listed first
     dired-listing-switches "-lah --group-directories-first"
     dired-omit-files "^\\.?#\\|^\\.$\\|^\\.[^\\.].*$"
     dired-omit-verbose nil
     dired-recursive-copies 'always
     dired-recursive-deletes 'always)
    (add-hook 'dired-mode-hook
	      (lambda ()
		(load "dired-x" nil t)
		(dired-omit-mode 1)
		(auto-revert-mode t))
              ;; keybindings
              (evil-define-key 'normal dired-mode-map ")"
                'dired-details-toggle))
    ;; TODO: is this still needed?
    (add-hook 'dired-load-hook (lambda ()
				 (load "dired-x")))))

(use-package ispell
  :init
  (progn
    (use-package flyspell)
    (use-package auto-dictionary
      :ensure auto-dictionary)
    (add-hook 'flyspell-mode-hook
	      (lambda ()
		(auto-dictionary-mode 1)
		;; keybindings
		(define-key flyspell-mode-map (kbd "M-TAB") nil)
		(define-key flyspell-mode-map (kbd "C-<tab>") 
		  'flyspell-auto-correct-word)))))


;; coq
(use-package proof-site
  :init
  (progn
    (add-hook 'coq-mode-hook
  	      (lambda ()
  		(setq
                 coq-compile-before-require t
                 coq-one-command-per-line nil
                 coq-script-indent nil
                 proof-find-theorems-command "SearchAbout %s"
                 proof-auto-action-when-deactivating-scripting (quote retract)
                 proof-electric-terminator-enable nil
                 proof-follow-mode (quote ignore)
                 proof-imenu-enable nil
                 proof-script-fly-past-comments t
                 proof-strict-read-only t
                 proof-three-window-enable t)))))

(use-package agda2
  :init
  (progn
    (setq agda2-include-dirs '("." "./lib/src"))
    (add-hook 'agda2-mode-hook
              (lambda ()
                (dolist (th custom-enabled-themes) (disable-theme th))))))


(use-package haskell-mode
  :ensure haskell-mode
  ;; TODO: should work without pair, but simply the pattern (see github docs)
  :mode ("\\.l?hs\\'" . haskell-mode)
  :init (add-hook 'haskell-mode-hook
		  (lambda ()
		    ;; switches
		    (use-package hi2
		      :ensure hi2)
		    (turn-on-hi2)
		    ;;(turn-on-haskell-simple-indent)
		    ;; keybindings
		    (evil-define-key 'insert haskell-mode-map (kbd "RET") 'newline)
		    (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
		    (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
		    (evil-define-key 'normal haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
		    (evil-define-key 'normal haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
		    (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
		    (evil-define-key 'normal haskell-mode-map [f8] 'haskell-navigate-imports)
		    )))

;; TODO: make a melpa package
(use-package ottmode)
