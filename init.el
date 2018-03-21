;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Change some default settings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq user-full-name "Mikołaj Gałkowski")									; My name
(setq gc-cons-threshold (* 500 1024 1024))									; increase the threshold for garbage collection - 100 MB
(setq delete-old-versions -1)											; delete excess backup versions silently
(setq delete-old-versions -1)											; delete excess backup versions silently
(setq version-control t)											; use version control for backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))							; which directory to put backups file
(setq inhibit-startup-screen t)											; inhibit useless and old-school startup screen
(setq visible-bell nil)												; no visible bell for errors
(setq auto-hscroll-mode t)												; no visible bell for errors
(setq ring-bell-function 'ignore)										; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)										; use utf-8 by default for reading
(setq coding-system-for-write 'utf-8)										; use utf-8 by default for writing
(setq initial-major-mode 'fundamental-mode)									; set the mode of the initial scratch buffer
(setq initial-scratch-message "")           									; print nothing and leave screen at insert mode
(menu-bar-mode -1)												; deactivate the menubar
(tool-bar-mode -1)												; deactivate the toolbar
(scroll-bar-mode -1)												; deactivate the scrollbar
(tooltip-mode -1)												; deactivate the tooltip
(defun display-startup-echo-area-message () (message "Good morning, Logic is always number 1")) 		; change the default startup echo message
(setq-default truncate-lines t)											; always truncate lines ;hello
(setq large-file-warning-threshold (* 15 1024 1024))								; increase theshold for larger files
(fset 'yes-or-no-p 'y-or-n-p)											; prompt for 'y' or 'n' instead of 'yes' or 'no'
(setq-default abbrev-mode t)											; turn on abbreviations by default
(setq recenter-positions '(middle top bottom))								  	; recenter from the top instead of the middle
(put 'narrow-to-region 'disabled nil)										; enable narrowing to region
(put 'narrow-to-defun 'disabled nil)										; enable narrowing to function
(when (fboundp 'winner-mode)											; when you can find 'winner-mode'
  (winner-mode 1))												; activate winner mode
(setq recentf-max-saved-items 1000										; set the number of recent items to be saved
      recentf-exclude '("/tmp/" "/ssh:"))									; exclude the temporary and remote files accessed recently
(setq enable-recursive-minibuffers t)										; use the minibuffer while using the minibuffer
(setq echo-keystrokes 0.05)											; when to echo keystrokes
(setq frame-resize-pixelwise t)											; resize based on pixels to remove annoying gaps
(setq-default tab-width 2)			   								; default tab width
(show-paren-mode 1)											; hightlight pharentheses and shit
(setq x-super-keysym 'meta)							;use super as meta
(electric-pair-mode)										; automatically close brackets and shit
(setq shell-file-name "bash")						; shell name to bash
(setq shell-command-switch "-ci")				; use my .bashrc aliases
;; (setq debug-on-error t)
;; (setq split-width-threshold 'nil)

;; ANSI colors in shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(4 ((shift) . 4))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 4) ;; keyboard scroll one line at a time



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Package management    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Include the module to install packages
(require 'package)

;; tells emacs not to load any packages before starting up
(setq package-enable-at-startup nil)

;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "https://orgmode.org/elpa/")
												 ("gnu"       . "https://elpa.gnu.org/packages/")
												 ("melpa"     . "https://melpa.org/packages/")))

;; initialize the packages
(setq package-check-signature nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;
;;    use-package    ;;
;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)       

(use-package diminish
  :ensure t
  :defer t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Built-in packages    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; pdf/image viewing
(use-package doc-view
  :hook ((doc-view-mode . doc-view-fir-page-to-window)
         (doc-view-minor-mode . doc-view-fir-page-to-window))
  :init
  (setq doc-view-continuous t))

;; builtin version control
(use-package vc
  :init
  (setq vc-make-backup-files t
        vc-follow-symlinks t))

;; diff management
(use-package ediff
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;; abbrev
(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq save-abbrevs 'silently)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


;; auto fill mode
(use-package simple
  :diminish auto-fill-function)

;; auto revert mode
(use-package autorevert
  :defer t
  :diminish auto-revert-mode)

;; documentation helper
(use-package eldoc
  :hook (prog-mode . eldoc-mode)
  :diminish eldoc-mode)

;; spell check
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (org . flyspell-mode))
  :diminish (flyspell-mode . " φ"))

;; save history
(use-package savehist
  :defer t
  :config
  (savehist-mode 1))


(use-package fzf
	:defer t
	:config
	(setq fzf/directory-start "/")
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Third party packages    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; extra functions
(add-to-list 'load-path
						 (expand-file-name "defuns" user-emacs-directory))

;; other configuration
(add-to-list 'load-path
						 (expand-file-name "config" user-emacs-directory))

;; manually installed packages
(add-to-list 'load-path
						 (expand-file-name "local" user-emacs-directory))



;; hint for bindings
(use-package which-key
  :ensure t
  :demand t
  :diminish which-key-mode
  :bind* (("C-c ?" . which-key-show-top-level))
  :config
  ;; workaround for emacs 26
  (if (version< emacs-version "26")
      (message "Tracking stable Emacs")
    (defalias 'display-buffer-in-major-side-window 'window--make-major-side-window))
  ;; turn on which key and add some names for default/common prefixes
  (which-key-enable-god-mode-support)
  (which-key-mode))


(use-package restart-emacs
  :ensure t
  :bind* (("C-x C" . restart-emacs)))

;;configs
(require 'mikus-evil)

(require 'mikus-hydra)


;; web mode config - eslint, babel, react and shit


(require 'mikus-webmode)

;;; Shell mode

;;; helm sys
(require 'helm-sys)




;; autocompletion

(use-package company
  :ensure t
	:init
	(global-company-mode)
	:config 
	(add-to-list 'company-backends '( company-tern company-css company-files))
	(add-to-list 'company-dabbrev-code-modes '(web-mode json-mode))
	(setq company-tooltip-align-annotations t)
	)
	

(use-package smooth-scroll
  :ensure t
	:config
	(smooth-scroll-mode)
	)


(use-package org
	:ensure t
	:config
	(global-set-key "\M-n" 'org-metadown)
	(global-set-key "\M-p" 'org-metaup)
	(global-set-key "\M-ol" 'org-store-link)
	(global-set-key "\M-oa" 'org-agenda)
	(global-set-key "\M-oc" 'org-capture)
	(global-set-key "\M-ob" 'org-iswitchb)
	)

;; defuns 

(require 'reindent-buffer)



;; colortheme

(use-package gruvbox-theme
  :ensure t
	:init
	(load-theme 'gruvbox-dark-hard t)
	)


;; magit package

(use-package magit 
  :ensure t) 

;; exex path from shell to fix stuff with bash and shit

;; (use-package exec-path-from-shell
;;   :ensure t
;; 	:init
;; 	(exec-path-from-shell-initialize)
;; 	) 



;;; unset some keys
(global-set-key (kbd "<f3>") nil)
 

;; Use ranger dired extension for best file management

(use-package ranger 
	:ensure t
  :commands (ranger)
  :bind (("<f3>" . ranger))
  :init
  (setq
   ranger-show-hidden nil
   ranger-override-dired-mode t)) 

(with-eval-after-load 'ranger
  (define-key ranger-normal-mode-map "k" 'ranger-next-file)
  (define-key ranger-normal-mode-map "l" 'ranger-prev-file) 
  (define-key ranger-normal-mode-map "j" 'ranger-up-directory) 
  (define-key ranger-mode-map ":" ranger-dired-map) 
  (define-key ranger-normal-mode-map (kbd "h") 'ranger-goto-mark)
  (define-key ranger-mode-map ";" 'ranger-find-file)
  (define-key ranger-normal-mode-map ";" 'ranger-find-file) 
  ) 











;; This is automatically generated

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("81db42d019a738d388596533bd1b5d66aef3663842172f3696733c0aab05a150" default)))
 '(package-selected-packages
	 (quote
		(org-mode indium ace-window hydra helm-register helm-top helm-projectile json-mode helm-config helm fzf evil-snipe evil-easymotion avy xterm-color smooth-scroll exec-path-from-shell evil-matchit evil-mathit evil-repeat company-tern tern-company company company-mode js2-mode babel-eslint eslint-plugin-react eslint flycheck exwm exwm-x ranger magit web-mode gruvbox-theme evil-visualstar evil-mc evil-nerd-commenter evil-surround evil-leader evil restart-emacs which-key diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
