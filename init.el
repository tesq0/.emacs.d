;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Change some default settings    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq user-full-name "Mikołaj Gałkowski")                 ; My name
(setq gc-cons-threshold (* 500 1024 1024))                  ; increase the threshold for garbage collection - 100 MB
(setq delete-old-versions -1)                     ; delete excess backup versions silently
(setq delete-old-versions -1)                     ; delete excess backup versions silently
(setq version-control t)                      ; use version control for backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))             ; which directory to put backups file
(setq inhibit-startup-screen t)                     ; inhibit useless and old-school startup screen
(setq visible-bell nil)                       ; no visible bell for errors
(setq auto-hscroll-mode t)                        ; no visible bell for errors
(setq ring-bell-function 'ignore)                   ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)                    ; use utf-8 by default for reading
(setq coding-system-for-write 'utf-8)                   ; use utf-8 by default for writing
(setq initial-major-mode 'fundamental-mode)                 ; set the mode of the initial scratch buffer
(setq initial-scratch-message "")                             ; print nothing and leave screen at insert mode
(menu-bar-mode -1)                        ; deactivate the menubar
(tool-bar-mode -1)                        ; deactivate the toolbar
(scroll-bar-mode -1)                        ; deactivate the scrollbar
(tooltip-mode -1)                       ; deactivate the tooltip
(defun display-startup-echo-area-message () (message "Good morning, Logic is always number 1"))     ; change the default startup echo message
(setq-default truncate-lines t)                     ; always truncate lines ;hello
(setq large-file-warning-threshold (* 15 1024 1024))                ; increase theshold for larger files
(fset 'yes-or-no-p 'y-or-n-p)                     ; prompt for 'y' or 'n' instead of 'yes' or 'no'
(setq-default abbrev-mode t)                      ; turn on abbreviations by default
(setq recenter-positions '(middle top bottom))                    ; recenter from the top instead of the middle
(put 'narrow-to-region 'disabled nil)                   ; enable narrowing to region
(put 'narrow-to-defun 'disabled nil)                    ; enable narrowing to function
(when (fboundp 'winner-mode)                      ; when you can find 'winner-mode'
  (winner-mode 1))                        ; activate winner mode
(setq enable-recursive-minibuffers t)                   ; use the minibuffer while using the minibuffer
(setq echo-keystrokes 0.05)                     ; when to echo keystrokes
(setq frame-resize-pixelwise t)                     ; resize based on pixels to remove annoying gaps
(setq-default tab-width 2)                        ; default tab width
(show-paren-mode 1)                     ; hightlight pharentheses and shit
(setq x-super-keysym 'meta)             ;use super as meta
(setq shell-file-name "bash")           ; shell name to bash
(setq shell-command-switch "-c")        ; use my .bashrc aliases
(setq initial-buffer-choice t)
;; (server-start)
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

;; ;; documentation helper
;; (use-package eldoc
;;   :hook (prog-mode . eldoc-mode)
;;   :diminish eldoc-mode)

;; spell check 
(use-package flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . nil))
  :init (progn
          ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
          (dolist (mode-hook '(text-mode-hook org-mode-hook LaTeX-mode-hook))
            (add-hook mode-hook #'flyspell-mode))))

;; save history
(use-package savehist
  :defer t
  :config
  (savehist-mode 1))


(use-package fzf
  :defer t
  :config 
	(progn
		(setq fzf/directory-start "/")
		)
	)

(use-package term
						 :config
						 (progn 
							 (define-key term-mode-map "\C-n" 'term-next-input)
							 (define-key term-mode-map "\C-p" 'term-previous-input)
							 )
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


(use-package async
  :ensure t
  :config 
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)
  )


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



;; flash current line

(require 'hl-line+)
(global-set-key (kbd "<C-return>") 'hl-line-flash)

;; web mode config - eslint, babel, react and shit


(require 'mikus-webmode)

;;; Shell mode

;;; helm sys
(require 'helm-sys)



(use-package buffer-move
  :bind (("<s-up>" . buf-move-up)
         ("<s-down>" . buf-move-down)
         ("<s-left>" . buf-move-left)
         ("<s-right>" . buf-move-right)))


;; autocompletion

(use-package company
  :init (progn
          (add-hook 'prog-mode-hook 'company-mode))
  :config (progn
            (setq company-idle-delay 0.5)
            (setq company-tooltip-limit 10)
            (setq company-minimum-prefix-length 2)
            (setq company-tooltip-flip-when-above t)
            (add-to-list 'company-backends '( company-keywords company-capf company-files))))

(use-package company-dabbrev
  :config (progn
            (setq company-dabbrev-ignore-case t)
            (setq company-dabbrev-downcase nil)))

;; (use-package ediff
;;   :config (progn
;;             ;; window positioning & frame setup
;;             (setq ediff-window-setup-function 'ediff-setup-windows-plain
;;                   ediff-split-window-function 'split-window-horizontally)))


(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

(use-package elec-pair
  :ensure t
  :config (electric-pair-mode t))

  
(use-package company-tern
  :ensure t
  )

(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode)
  )


;; (use-package multiple-cursors
;;   :bind (("M-RET" . mc/edit-lines)
;;          ("C-<" . mc/mark-previous-like-this)
;;          ("C->" . mc/mark-next-like-this)
;;          ("C-M-<" . mc/unmark-next-like-this)
;;          ("C-M->" . mc/unmark-previous-like-this)
;;          ("C-c C-<" . mc/mark-all-like-this)))


(use-package rainbow-mode
  :ensure t
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
(load-theme 'deeper-blue)

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
  :ensure t) 

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
 

;; Use ranger dired extension for best file management

(use-package ranger 
  :ensure t
  :commands (deer)
  :bind (("<f3>" . deer))
  :init
  (setq
   ranger-show-hidden nil
   ranger-override-dired-mode t
   ranger-cleanup-on-disable t)
	(add-hook 'ranger-mode-hook
						(lambda ()
							(local-unset-key "\C-f")
							(local-unset-key "\C-b")
							(setq-local helm-descbinds-window-style 'same-window 
										helm-split-window-inside-p nil ; make helm occupy window, just for ranger
										)))
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

(global-set-key (kbd "C-f") 'ctl-x-5-prefix)
(define-key ctl-x-5-map (kbd "n") 'vmake-frame)
(define-key ctl-x-5-map (kbd "N") 'hmake-frame)
(define-key ctl-x-5-map (kbd "b") 'switch-to-buffer-other-frame)
(define-key ctl-x-5-map (kbd "o") 'delete-other-frames)
(define-key ctl-x-5-map (kbd "c") 'delete-frame)
(define-key ctl-x-5-map (kbd "f") 'find-file)
(define-key ctl-x-5-map (kbd "C-i") 'other-frame)
(global-set-key (kbd "C-c C-e") 'eval-buffer)








(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("bce3ae31774e626dce97ed6d7781b4c147c990e48a35baedf67e185ebc544a56" "75c5c39809c52d48cb9dcbf1694bf2d27d5f6fd053777c194e0b69d8e49031c0" default)))
 '(package-selected-packages
	 (quote
		(hl-line+ nav-flash zerodark-theme xterm-color xref-js2 which-key web-mode use-package tide smooth-scrolling smooth-scroll rjsx-mode restart-emacs ranger rainbow-mode json-mode js2-refactor js-doc ivy indium hydra helm-projectile gruvbox-theme fzf exwm exec-path-from-shell evil-visualstar evil-surround evil-snipe evil-nerd-commenter evil-mc evil-matchit evil-leader evil-easymotion editorconfig diminish company-tern ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
