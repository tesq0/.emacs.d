(setq user-full-name "Mikołaj Gałkowski")                 ; My name
;;(setq gc-cons-threshold (* 500 1024 1024))                  ; increase the threshold for garbage collection - 100 MB
;;(setq delete-old-versions -1)                     ; delete excess backup versions silently
;;(setq version-control t)                      ; use version control for backups
;;(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))             ; which directory to put backups file
(setq inhibit-startup-screen t)                     ; inhibit useless and old-school startup screen
(setq visible-bell nil)                       ; no visible bell for errors
(setq ring-bell-function 'ignore)                   ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)                    ; use utf-8 by default for reading
(setq coding-system-for-write 'utf-8)                   ; use utf-8 by default for writing
(setq initial-scratch-message "")                             ; print nothing and leave screen at insert mode


(defun display-startup-echo-area-message () (message "Jestem fajny"))     ; change the default startup echo message
(setq-default truncate-lines t)                     ; always truncate lines ;hello
(setq large-file-warning-threshold (* 15 1024 1024))                ; increase theshold for larger files
(fset 'yes-or-no-p 'y-or-n-p)                     ; prompt for 'y' or 'n' instead of 'yes' or 'no'
;; (setq-default abbrev-mode t)                      ; turn on abbreviations by default
(setq recenter-positions '(middle top bottom))                    ; recenter from the top instead of the middle
;; (put 'narrow-to-defun 'disabled nil)                    ; enable narrowing to function
(when (fboundp 'winner-mode)                      ; when you can find 'winner-mode'
  (winner-mode 1))                        ; activate winner mode
;;(setq enable-recursive-minibuffers t)                   ; use the minibuffer while using the minibuffer
(setq echo-keystrokes 0.05)                     ; when to echo keystrokes
;;(setq frame-resize-pixelwise t)                     ; resize based on pixels to remove annoying gaps
(setq-default tab-width 8)                        ; default tab width
(show-paren-mode 1)                     ; hightlight pharentheses and shit
;; (setq x-super-keysym 'meta)             ;use super as meta

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(defconst emacs-backup-dir "/tmp/backup-emacs")

(dolist (d (list emacs-tmp-dir emacs-backup-dir))
  (if (not (file-exists-p d))
      (make-directory d t)))

(setq backup-by-copying t)

(setq make-backup-files t)

(setq backup-inhibited nil)

(setq backup-directory-alist
      `((".*" . ,emacs-backup-dir)))

(setq auto-save-default t)

(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))

(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(setq create-lockfiles nil)

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq case-fold-search nil)
(setq initial-buffer-choice t)					; use scratchpad as default buffer when calling emacsclient

(setq bidi-paragraph-direction "left-to-right")
(setq bidi-inhibit-bpa t)

(global-so-long-mode 1)

(set-language-environment "UTF-8")

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq desktop-dirname (expand-file-name "save" user-emacs-directory))

(recentf-mode)

;; extra functions
(add-to-list 'load-path
	     (expand-file-name "defuns" user-emacs-directory))


;; TRAMP
;; (add-to-list 'tramp-default-proxies-alist
;;						 '(nil "\\`root\\'" "/ssh:%h:"))

;; (add-to-list 'tramp-default-proxies-alist
;;						 '((regexp-quote (system-name)) nil nil))


;; manually installed packages

(defun add-to-loadpath-recursive (dir)
  (let ((default-directory dir))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-loadpath-recursive (expand-file-name "site-lisp" user-emacs-directory))

(let ((nixos-lisp-path "/run/current-system/sw/share/emacs/site-lisp"))
  (when (file-exists-p nixos-lisp-path)
    (add-to-loadpath-recursive nixos-lisp-path)))

(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

(if (boundp 'constants) (require 'constants))

(defgroup init nil
  "Init packages config")

(require 'init-package)
(require 'init-const)
(require 'init-system)
(require 'init-ui)
(require 'init-utils)
(require 'init-gui-frames)
(require 'init-modeline)
(require 'init-general)
(require 'init-jump-to-def)
(require 'init-evil)
(require 'init-flycheck)
(require 'init-magit)
(require 'init-ibuffer)
(require 'init-dired)
(require 'init-company)
(require 'init-eldoc)
(require 'init-org)
(require 'init-csharp)
(require 'init-c)
(require 'init-lsp)
;; (require 'init-java)
(require 'init-kotlin)
(require 'init-webmode)
(require 'init-php)
(require 'init-hideshow)
(require 'init-hydra)
(require 'init-projectile)
(require 'init-helm)
(require 'init-search)
(require 'init-diff)
(require 'init-window)
(require 'init-mc)
(require 'init-dict)
(require 'init-mouse)
(require 'init-wgrep)
(require 'init-asm)
(require 'init-clojure)
(require 'init-tags)
(require 'init-nix)
(require 'init-smartparens)
(require 'init-vc)
(require 'init-dart)
(require 'init-mail)
(require 'init-godot)
(require 'init-tex)
(require 'init-arduino)
(require 'init-python)
(require 'init-spellcheck)


;; ERC client
(after-load 'erc-backend
  (progn
    (require 'erc-sasl)
    ;; e.g. irc\\.freenode\\.net, or .* for any host
    (add-to-list 'erc-sasl-server-regexp-list "irc\\.freenode\\.net")))


;; auto revert mode
(use-package autorevert
  :defer t
  :diminish auto-revert-mode)

(setq debug-on-error nil)

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



(require 'visual-basic-mode)

(defvar mikus-flash-timer nil)
(defvar point-before-jump nil)

(defun before-jump ()
  "Save current POINT before jumping to another location."
  (interactive)
  (setq point-before-jump (point-at-bol)))

(defun my/hl-line ()
  "Highlight line at point."
  (interactive)
  (when (not (eq (point-at-bol) point-before-jump) ) ;; if at different point than before the jump
    (isearch-highlight (point-at-bol) (point-at-eol))
    (when mikus-flash-timer
      (cancel-timer mikus-flash-timer))
    (setq mikus-flash-timer
	  (run-at-time 0.5 nil 'isearch-dehighlight))))

(defun try-hl-line-after-file-opened (buffer &rest args)
  "Try to highlight line after switching to a BUFFER."
  (when buffer
    (my/hl-line)))

(advice-add 'switch-to-buffer :after 'try-hl-line-after-file-opened )
(advice-add 'evil-jump-backward :before 'before-jump)
(advice-add 'evil-jump-forward :before 'before-jump)
(advice-add 'evil-jump-backward :after 'my/hl-line )
(advice-add 'evil-jump-forward :after 'my/hl-line )
;; (advice-remove 'evil-jump-backward 'try-hl-line-after-file-opened )

;; flash current line
(global-set-key (kbd "<C-return>") 'my/hl-line)


;; better help

(require 'help-fns+)

;;; helm sys
(require 'helm-sys)


(use-package editorconfig
  :ensure t
  :init (editorconfig-mode 1))

(use-package elec-pair
  :ensure t
  :config (electric-pair-mode t))

(use-package darkroom
  :ensure t
  :init
  (progn
    (require 'darkroom)
    (setq darkroom-text-scale-increase 1)
    (general-define-key
     "<C-f11>" 'darkroom-mode)
    )
  )

;; (use-package company-tern
;; 	:ensure t
;; 	)

(use-package rainbow-mode
  :ensure t)

;; defuns

(require 'reindent-buffer)


(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; override this fucking shit ESC
(define-key ctl-x-map (kbd "<ESC>" ) nil)

(define-prefix-command	'toggle-map)
(global-set-key (kbd "C-c o") 'toggle-map)

(general-define-key
 :keymaps 'toggle-map
 "l" 'linum-mode
 "t" 'toggle-truncate-lines
 "d" 'toggle-debug-on-error)

(define-prefix-command	'fast-ex-map)
(mikus-leader "x" 'fast-ex-map)
(define-key fast-ex-map (kbd "e") 'aweshell-new)
(define-key fast-ex-map (kbd "f") 'explorer)
(define-key fast-ex-map (kbd "p") 'power-shell)
(define-key fast-ex-map (kbd "t") 'terminal)

(define-key ctl-x-map (kbd "D") 'ranger)

(global-set-key (kbd "C-f") 'ctl-x-5-prefix)
;;(evil-leader/set-key "f" 'ctl-x-5-prefix)
(define-key ctl-x-5-map (kbd "n") 'make-frame)
;; (define-key ctl-x-5-map (kbd "N") 'hmake-frame)
(define-key ctl-x-5-map (kbd "b") 'switch-to-buffer-other-frame)
(define-key ctl-x-5-map (kbd "o") 'delete-other-frames)
(define-key ctl-x-5-map (kbd "c") 'delete-frame)
(define-key ctl-x-5-map (kbd "f") 'find-file)
(define-key ctl-x-5-map (kbd "C-i") 'other-frame)
(global-set-key (kbd "C-c C-e") 'eval-buffer)
(global-set-key (kbd "C-h h") nil) ;; disable that shitty hello file
(define-key ctl-x-map (kbd "C-h") 'help-command)

(global-set-key (kbd "<C-escape>") 'keyboard-quit)

(define-prefix-command	'fast-buffer-map)

(define-prefix-command	'convert-case-map)
(global-set-key (kbd "C-c c") 'convert-case-map)
(define-key convert-case-map (kbd "b") 'camel-to-burger-case)

(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))

;; Annoying undo tree keybindings
(general-unbind
  :keymaps 'undo-tree-map
  "C-_" nil
  "C-/" nil
  "C-?" nil
  "M-_" nil)

(general-unbind
  "C-_" nil
  "C-/" nil
  "C-?" nil
  "M-_" nil)

(after-load 'imenu
  (setq imenu-auto-rescan t))

(require 'csharp-hs-forward-sexp)

(use-package markdown-mode
  :ensure t)

(use-package ahk-mode
  :ensure t)

(use-package mmm-mode
  :commands mmm-mode
  :config
  (use-package mmm-auto
    :ensure nil))

(use-package sonic-pi
  :ensure t
  :init
  (progn
    (setq sonic-pi-server-bin "App\\Sonic Pi\\app\\server\\ruby\\bin\\sonic-pi-server.rb")
    (setq sonic-pi-compile-extensions-bin "App\\Sonic Pi\\app\\server\\ruby\\bin\\compile-extensions.rb")
    (setq sonic-pi-path "D:\\Programs\\SonicPiPortable\\")))

(use-package dockerfile-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package aweshell
  :quelpa (aweshell :fetcher github :repo "manateelazycat/aweshell")
  :init
  (progn
    (require 'aweshell)

    (defun load-aweshell-company-bindings ()
      (general-unbind company-active-map "RET"))
    
    (defun unload-aweshell-company-bindings ()
      (message "UNLOAD")
      (general-define-key :keymaps 'company-active-map
			  "RET" 'company-complete-selection))

    (defun handle-loadin-eshell-keybindings ()
      (message "Handle loading eshell keybindings")
      (if (eshell-mode)
	  (load-aweshell-company-bindings)
	(unload-aweshell-company-bindings)))
    
    (defun setup-aweshell-keybindings ()
      (load-aweshell-company-bindings)
      (general-define-key
       :keymap 'eshell-mode-map
       "C-n" 'eshell-next-input
       "C-p" 'eshell-previous-input
       "C-c c" 'aweshell-clear-buffer
       "C-c n" 'aweshell-next
       "C-c p" 'aweshell-prev
       "C-c s" 'aweshell-sudo-toggle
       "C-c x" 'aweshell-new))
    )
  )

(use-package yaml-mode
  :ensure t)

(use-package firestarter
  :ensure t
  :init
  (firestarter-mode))

(use-package pomodoro
  :ensure t
  :init
  (progn
    (setq pomodoro-sound-player "mpv"
	  pomodoro-desktop-notification t))
  :config
  (pomodoro-add-to-mode-line))

(use-package edit-server
  :ensure t)

(use-package direnv
  :config
  (direnv-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :init (progn
	  (add-to-list 'company-backends #'company-yasnippet))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)
(put 'narrow-to-region 'disabled nil)
