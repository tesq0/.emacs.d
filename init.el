;; My name
(setq user-full-name "Mikołaj Gałkowski")

;; inhibit useless and old-school startup screen
(setq inhibit-startup-screen t)

;; no visible bell for errors
(setq visible-bell nil)

;; silent bell when you make a mistake
(setq ring-bell-function 'ignore)

;; use utf-8 by default for reading
(setq coding-system-for-read 'utf-8)

;; use utf-8 by default for writing
(setq coding-system-for-write 'utf-8)                   

;; print nothing and leave screen at insert mode
(setq initial-scratch-message "")

;; always truncate lines ;hello
(setq-default truncate-lines t)

;; increase theshold for larger files
(setq large-file-warning-threshold (* 15 1024 1024))

;; prompt for 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; recenter from the top instead of the middle
(setq recenter-positions '(middle top bottom))

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; when to echo keystrokes
(setq echo-keystrokes 0.05)

;; default tab width
(setq-default tab-width 8)

;; hightlight pharentheses and shit
(show-paren-mode 1)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(defconst emacs-server-socket-dir (expand-file-name "socket" user-emacs-directory))
(defconst emacs-backup-dir "/tmp/backup-emacs")

(dolist (d (list emacs-tmp-dir emacs-backup-dir emacs-server-socket-dir))
  (if (not (file-exists-p d))
      (make-directory d t)))

(with-eval-after-load 'server
  (setq server-socket-dir emacs-server-socket-dir))

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

(add-to-loadpath-recursive (expand-file-name "vendor" user-emacs-directory))


(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

(if (boundp 'constants) (require 'constants))

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

(defconst gc-threshold 100000000)

;; Lower threshold back to gc-treshold
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold gc-threshold)))


(setq read-process-output-max (* 1024 1024))

(fido-vertical-mode)


(defgroup init nil
  "Init packages config")

(require 'init-const)
(require 'init-system)
(require 'init-ui)
(require 'init-utils)
(require 'init-modeline)
(require 'init-magit)
(require 'init-dired)
(require 'init-eldoc)
(require 'init-org)
(require 'init-c)
;; (require 'init-webmode)
(require 'init-search)
(require 'init-diff)
(require 'init-mouse)
;; (require 'init-clojure)
;; (require 'init-tags)
;; (require 'init-vc)

(require 'init-macros)

(setq debug-on-error nil)

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

(advice-add 'switch-to-buffer :after 'try-hl-line-after-file-opened)

;; flash current line
(global-set-key (kbd "<C-return>") 'my/hl-line)

;; (add-hook 'prog-mode-hook editorconfig-mode)

(add-hook 'prog-mode-hook 'electric-pair-mode)

(global-set-key (kbd "RET") 'newline-and-indent)

;; override this fucking shit ESC
(define-key ctl-x-map (kbd "<ESC>" ) nil)

(defvar-keymap toggle-map
 "l" 'linum-mode
 "t" 'toggle-truncate-lines
 "d" 'toggle-debug-on-error)

(global-set-key (kbd "C-c t") 'toggle-map)

(define-prefix-command	'fast-ex-map)
(global-set-key (kbd "C-c x") 'fast-ex-map)
(define-key fast-ex-map (kbd "e") 'aweshell-new)
(define-key fast-ex-map (kbd "f") 'explorer)
(define-key fast-ex-map (kbd "p") 'power-shell)
(define-key fast-ex-map (kbd "t") 'terminal)

(global-set-key (kbd "C-f") 'ctl-x-5-prefix)
(define-key ctl-x-5-map (kbd "n") 'make-frame)
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

(define-prefix-command	'open-map)
(global-set-key (kbd "C-c o") 'open-map)

(global-set-key (kbd "<C-tab>") 'switch-to-the-window-that-displays-the-most-recently-selected-buffer)

(define-prefix-command	'insert-stuff-map)
(define-key insert-stuff-map (kbd "b") 'insert-buffer-basename)
(global-set-key (kbd "C-c i") 'insert-stuff-map)

(with-eval-after-load 'imenu
  (setq imenu-auto-rescan t))

(when (>= emacs-major-version 23)
  (defun server-ensure-safe-dir (dir) "Noop" t))

;; snippets
(autoload 'yas-insert-snippet "yasnippet")

(with-eval-after-load 'yasnippet
  (load 'yasnippet-snippets)
  (define-prefix-command 'my-snippet-map)
  (define-key 'my-snippet-map "i" 'yas-insert-snippet)
  (define-key 'my-snippet-map "n" 'yas-new-snippet)
  (define-key 'my-snippet-map "v" 'yas-visit-snippet-file)

  ;; Load custom snippets
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)

  (add-hook 'prog-mode-hook 'yas-minor-mode)

  (keymap-set yas-minor-mode-map (kbd "C-c C-s") my-snippet-map)
  (keymap-set yas-minor-mode-map (kbd "C-c s") my-snippet-map))


(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)
(put 'narrow-to-region 'disabled nil)
