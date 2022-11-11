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
(setq initial-buffer-choice nil)

(setq bidi-paragraph-direction "left-to-right")
(setq bidi-inhibit-bpa t)
(setq debug-on-error t)

(global-so-long-mode 1)

(set-language-environment "UTF-8")

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq desktop-dirname (expand-file-name "save" user-emacs-directory))

;; TRAMP
;; (add-to-list 'tramp-default-proxies-alist
;;						 '(nil "\\`root\\'" "/ssh:%h:"))

;; (add-to-list 'tramp-default-proxies-alist
;;						 '((regexp-quote (system-name)) nil nil))

;; manually installed packages

(if (boundp 'constants) (require 'constants))

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

(defconst gc-threshold 100000000)

;; Lower threshold back to gc-treshold
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold gc-threshold)))

(setq read-process-output-max (* 1024 1024))

;; External packages

(defvar +vendor-dir+ (expand-file-name "vendor" user-emacs-directory))
(defvar +loaddefs-path+ (expand-file-name "vendor-loaddefs.el" +vendor-dir+))

(defun add-to-loadpath-recursive (dir)
  (let ((default-directory dir))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-loadpath-recursive +vendor-dir+)

(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

(defgroup init nil
  "Init packages config")

(require 'init-const)
(require 'init-system)
(require 'init-ui)
(require 'init-utils)
(require 'init-modeline)
(require 'init-dired)
(require 'init-eldoc)
(require 'init-org)
(require 'init-c)
(require 'init-php)
;; (require 'init-webmode)
(require 'init-search)
(require 'init-diff)
(require 'init-mouse)
;; (require 'init-clojure)
(require 'init-tags)
(require 'init-vc)
(require 'init-yasnippet)
(require 'init-keybindings)
(require 'init-macros)

(recentf-mode)
(fido-vertical-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))

;; (add-hook 'prog-mode-hook editorconfig-mode)

(with-eval-after-load 'imenu
  (setq imenu-auto-rescan t))

(when (>= emacs-major-version 23)
  (defun server-ensure-safe-dir (dir) "Noop" t))


(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)
(put 'narrow-to-region 'disabled nil)
