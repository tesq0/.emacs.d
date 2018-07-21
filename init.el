;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Change some default settings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq user-full-name "Mikołaj Gałkowski")                 ; My name
;;(setq gc-cons-threshold (* 500 1024 1024))                  ; increase the threshold for garbage collection - 100 MB
;;(setq delete-old-versions -1)                     ; delete excess backup versions silently
;;(setq version-control t)                      ; use version control for backups
;;(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))             ; which directory to put backups file
(setq make-backup-files nil)						;Don't write backup files
(setq inhibit-startup-screen t)                     ; inhibit useless and old-school startup screen
(setq visible-bell nil)                       ; no visible bell for errors
(setq ring-bell-function 'ignore)                   ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)                    ; use utf-8 by default for reading
(setq coding-system-for-write 'utf-8)                   ; use utf-8 by default for writing
(setq initial-scratch-message "")                             ; print nothing and leave screen at insert mode


(defun display-startup-echo-area-message () (message "Good morning, Logic is always number 1"))     ; change the default startup echo message
(setq-default truncate-lines t)                     ; always truncate lines ;hello
(setq large-file-warning-threshold (* 15 1024 1024))                ; increase theshold for larger files
(fset 'yes-or-no-p 'y-or-n-p)                     ; prompt for 'y' or 'n' instead of 'yes' or 'no'
;; (setq-default abbrev-mode t)                      ; turn on abbreviations by default
(setq recenter-positions '(middle top bottom))                    ; recenter from the top instead of the middle
 ;; (put 'narrow-to-region 'disabled nil)                   ; enable narrowing to region
 ;; (put 'narrow-to-defun 'disabled nil)                    ; enable narrowing to function
(when (fboundp 'winner-mode)                      ; when you can find 'winner-mode'
	(winner-mode 1))                        ; activate winner mode
;;(setq enable-recursive-minibuffers t)                   ; use the minibuffer while using the minibuffer
(setq echo-keystrokes 0.05)                     ; when to echo keystrokes
;;(setq frame-resize-pixelwise t)                     ; resize based on pixels to remove annoying gaps
(setq-default tab-width 2)                        ; default tab width
(show-paren-mode 1)                     ; hightlight pharentheses and shit
;; (setq x-super-keysym 'meta)             ;use super as meta


(setq case-fold-search nil)
(setq initial-buffer-choice t)					; use scratchpad as default buffer when calling emacsclient

(setq user-emacs-directory (concat (getenv "HOME") "\\.emacs.d"))


(setq desktop-dirname (expand-file-name "save" user-emacs-directory))

;;(desktop-save-mode 1)
;;(add-hook after-make-frame-functions)

;; extra functions
(add-to-list 'load-path
						 (expand-file-name "defuns" user-emacs-directory))

;; manually installed packages
(add-to-list 'load-path
						 (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path
						 (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)
(require 'init-const)
(require 'init-system)
(require 'init-ui)
(require 'init-utils)
(require 'init-gui-frames)
(require 'init-search)
(require 'init-modeline)
(require 'init-general)
(require 'init-evil)
(require 'init-ivy)
(require 'init-magit)
(require 'init-ibuffer)
(require 'init-dired)
(require 'init-org)
(require 'init-company)
(require 'init-csharp)
(require 'init-hydra)
(require 'init-webmode)
(require 'init-hideshow)
(require 'init-projectile)

;; auto revert mode
(use-package autorevert
	:defer t
	:diminish auto-revert-mode)




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


;; flash current line

(global-set-key (kbd "<C-return>") 'hl-line-flash)


(require 'hl-line+)

;; better help

(require 'help-fns+)

;;; helm sys
(require 'helm-sys)


(use-package editorconfig
	:ensure t
	:config (editorconfig-mode 1))

(use-package elec-pair
	:ensure t
	:config (electric-pair-mode t))


(use-package company-tern
	:ensure t
	)


;; jump to definition

(use-package ag
	:ensure t )

(use-package dumb-jump
	:ensure t
	:config
	(global-set-key (kbd "M-.") 'dumb-jump-go)
	(global-set-key (kbd "M-,") 'dumb-jump-back)
	(add-hook 'dumb-jump-after-jump-hook 'hl-line-flash)
	)



(use-package rainbow-mode
	:ensure t)

;; defuns

(require 'reindent-buffer)


(global-set-key (kbd "RET") 'newline-and-indent)

;; override this fucking shit ESC
(define-key ctl-x-map (kbd "<ESC>" ) nil)

(define-prefix-command	'toggle-map)
(global-set-key (kbd "C-c o") 'toggle-map)
(define-key toggle-map (kbd "l") 'linum-mode)
(define-key toggle-map (kbd "t") 'toggle-truncate-lines)
(define-prefix-command	'fast-ex-map)
(mikus-leader
 :states 'normal
 :keymaps 'override
 "x" 'fast-ex-map
 )
(define-key fast-ex-map (kbd "e") 'shell-other-window)

(global-set-key (kbd "C-f") 'ctl-x-5-prefix)
;;(evil-leader/set-key "f" 'ctl-x-5-prefix)
(define-key ctl-x-5-map (kbd "n") 'vmake-frame)
(define-key ctl-x-5-map (kbd "N") 'hmake-frame)
(define-key ctl-x-5-map (kbd "b") 'switch-to-buffer-other-frame)
(define-key ctl-x-5-map (kbd "o") 'delete-other-frames)
(define-key ctl-x-5-map (kbd "c") 'delete-frame)
(define-key ctl-x-5-map (kbd "f") 'find-file)
(define-key ctl-x-5-map (kbd "C-i") 'other-frame)
(global-set-key (kbd "C-c C-e") 'eval-buffer)


(require 'csharp-hs-forward-sexp)


(use-package markdown-mode
	:ensure t)


;; snippets
(use-package yasnippet
	:ensure t
	:config
	(setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(define-prefix-command 'yas-map)
(global-set-key (kbd "C-y") 'yas-map)
(define-key yas-map (kbd "i") 'yas-insert-snippet)


(use-package perspective
	:ensure t)


(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)
