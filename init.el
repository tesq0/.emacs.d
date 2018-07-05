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


(setq initial-buffer-choice t)					; use scratchpad as default buffer when calling emacsclient


(defmacro local-require (pkg)
  `(load (file-truename (format "~/.emacs.d/site-lisp/%s/%s" ,pkg ,pkg))))

(defmacro require-init (pkg)
  `(load (file-truename (format "~/.emacs.d/lisp/%s" ,pkg))))


(let ((file-name-handler-alist nil))

	(require-init 'init-package)
	(require-init 'init-const)
	(require-init 'init-ui)
	(require-init 'init-gui-frames)
	(require-init 'init-modeline)
	(require-init 'init-evil)
	(require-init 'init-helm)
	(require-init 'init-hydra)
	(require-init 'init-csharp)
	(require-init 'init-webmode)

	(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

	)


;; auto revert mode
(use-package autorevert
	:defer t
	:diminish auto-revert-mode)



(setq user-emacs-directory (concat (getenv "HOME") "\\.emacs.d"))
;; extra functions
(add-to-list 'load-path
						 (expand-file-name "defuns" user-emacs-directory))

;; manually installed packages
(add-to-list 'load-path
						 (expand-file-name "site-lisp" user-emacs-directory))




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

(require 'hl-line+)
(global-set-key (kbd "<C-return>") 'hl-line-flash)

;;; Shell mode


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


(use-package org
	:ensure t)

;; defuns

(require 'reindent-buffer)
(require 'utils)


;; (require 'dracula-theme)


;;colortheme
;; (load-theme 'dracula t)

;; (if (daemonp)
;;		(add-hook 'after-make-frame-functions
;;							(lambda (frame)
;;								(with-selected-frame frame
;;									(load-theme 'dracula t))))
;;	(load-theme 'dracula t))


;; (use-package gruvbox-theme
;;   :ensure t
;;  :init
;;  (load-theme 'gruvbox-dark-hard t)
;;  )

;;   :ensure t
;;   :config
;;   (load-theme 'zerodark t)
;;   (zerodark-setup-modeline-format)
;;   )

;; (use-package zerodark-theme
;;   :ensure t
;;   :config
;;   (progn
;;     (defun set-selected-frame-dark ()
;;       (interactive)
;;       (let ((frame-name (cdr (assq 'name (frame-parameters (selected-frame))))))
;;         (call-process-shell-command
;;          (format
;;           "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT 'dark' -name '%s'"
;;           frame-name))))

;;     (when (window-system)
;;       (load-theme 'zerodark t)
;;       (zerodark-setup-modeline-format)
;;       (set-selected-frame-dark)
;;       (setq frame-title-format '(buffer-file-name "%f" ("%b"))))))

;; magit package

(use-package magit
	:ensure t
	:config
	(define-key magit-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
	(define-key magit-mode-map (kbd "C-g") 'magit-mode-bury-buffer)
	;; SMERGE
	(define-key smerge-mode-map (kbd "C-c m") (lookup-key smerge-mode-map (kbd "C-c ^")))
	)



(use-package fzf
	:ensure t)


(defun byte-compile-emacs ()
	"A function to byte compile Emacs dir."
	(interactive)
	(byte-recompile-directory (expand-file-name user-emacs-directory) 0))

;; exex path from shell to fix stuff with bash and shit

;; (use-package exec-path-from-shell
;;   :ensure t
;;  :init
;;  (exec-path-from-shell-initialize)
;;  )

(defun sudo-edit (&optional arg)
	"Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
	(interactive "P")
	(if (or arg (not buffer-file-name))
			(find-file (concat "/sudo:root@localhost:"
												 (ido-read-file-name "Find file(as root): ")))
		(find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; unset some keys
(global-set-key (kbd "<f3>") nil)
(global-unset-key (kbd "C-z"))



;; TEXT FOLDING


(use-package hideshow
	:ensure t
	:config
	(defvar hs-special-modes-alist
	(mapcar 'purecopy
	'((c-mode "{" "}" "/[*/]" nil nil)
		(c++-mode "{" "}" "/[*/]" nil nil)
		(bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
		(java-mode "{" "}" "/[*/]" nil nil)
		(js-mode "{" "}" "/[*/]" nil))))

	(define-prefix-command	'fold-prefix)

	(evil-leader/set-key "/" 'toggle-hiding)
	(add-hook 'c-mode-common-hook   'hs-minor-mode)
	(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
	(add-hook 'java-mode-hook       'hs-minor-mode)
	(add-hook 'lisp-mode-hook       'hs-minor-mode)
	(add-hook 'perl-mode-hook       'hs-minor-mode)
	(add-hook 'sh-mode-hook         'hs-minor-mode)
	(add-hook 'csharp-mode-hook      'hs-minor-mode)
	(add-hook 'web-mode-hook         'hs-minor-mode)
	(evil-leader/set-key "2" 'fold-prefix)

	(define-key fold-prefix (kbd "t") 'toggle-hiding)
	(define-key fold-prefix (kbd "s") 'hs-show-block)
	(define-key fold-prefix (kbd "h") 'hs-hide-block)
	(define-key fold-prefix (kbd "S") 'hs-show-all)
	(define-key fold-prefix (kbd "H") 'hs-hide-all)
	(define-key fold-prefix (kbd "l") 'hs-hide-level)
	(define-key fold-prefix (kbd "L") 'hs-hide-level-recursive)


	)


;; Use ranger dired extension for best file management

(use-package ranger
	:ensure t
	:commands (deer)
	:bind (("<f3>" . deer))
	:init
	(setq
	 ranger-show-hidden nil
	 ranger-override-dired-mode t
	 ranger-cleanup-on-disable t
	 ranger-cleanup-eagerly t
	 )
	(add-hook 'ranger-mode-hook
						(lambda ()
							(local-unset-key "\C-f")
							(local-unset-key "\C-b")
							(local-unset-key "\C-h")))
	:config
	(define-key ranger-mode-map "C-f" 'nil)
	(define-key ranger-mode-map "C-h" 'nil)
	(define-key ranger-mode-map "C-b" 'nil)
	(define-key ranger-normal-mode-map "k" 'ranger-next-file)
	(define-key ranger-normal-mode-map "l" 'ranger-prev-file)
	(define-key ranger-normal-mode-map "j" 'ranger-up-directory)
	(define-key ranger-mode-map ":" ranger-dired-map)
	(define-key ranger-normal-mode-map (kbd "h") 'ranger-goto-mark)
	(define-key ranger-mode-map ";" 'ranger-find-file)
	(define-key ranger-normal-mode-map ";" 'ranger-find-file)
	(define-key ranger-mode-map (kbd "C-g") 'ranger-close)
	(define-key ranger-mode-map (kbd "<escape>") 'ranger-close)
	(define-key ranger-mode-map (kbd "<f7>") 'dired-create-directory)
	(define-key ranger-mode-map (kbd "<f8>") 'dired-do-delete)
	(define-key ranger-mode-map (kbd "cw") 'dired-do-rename)
	(define-key ranger-mode-map (kbd "cm") 'dired-do-chmod)
	(define-key ranger-mode-map (kbd "cx") 'dired-do-compress)

	)

;; let's define some ghetoo keybindings

;; (define-prefix-command	'frame-map)
(defun vmake-frame ()
	"Make an Emacs horizontal frame in i3 window manager."
	(interactive)
	(shell-command "i3-msg split v")
	(make-frame))
(defun hmake-frame ()
	"Make an Emacs horizontal frame in i3 window manager."
	(interactive)
	(shell-command "i3-msg split h")
	(make-frame))

;; override this fucking shit ESC
(define-key ctl-x-map (kbd "<ESC>" ) nil)

(define-prefix-command	'toggle-map)
(global-set-key (kbd "C-c o") 'toggle-map)
(define-key toggle-map (kbd "l") 'linum-mode)
(define-prefix-command	'fast-ex-map)
(evil-leader/set-key "x" 'fast-ex-map)
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

(define-prefix-command 'mikus-search-map)
(evil-leader/set-key "s" 'mikus-search-map)
(define-key mikus-search-map (kbd "f") 'fzf-directory)
(define-key mikus-search-map (kbd "g") 'projectile-grep)
(define-key mikus-search-map (kbd "a") 'projectile-ag)


(require 'csharp-hs-forward-sexp)

;; languages config
(use-package common-lisp-snippets
	:ensure t )
(use-package slime
	:ensure t
	:config
	(setq inferior-lisp-program "/bin/sbcl"))
(use-package cl-lib
	:ensure t)
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



;; (define-prefix-command 'space-map)
;; (global-set-key (kbd "SPC") 'space-map)

(define-prefix-command 'helm-utils-map)
(evil-leader/set-key "h" 'helm-utils-map)
(define-key helm-utils-map (kbd "c") 'helm-colors)

(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)
