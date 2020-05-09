(use-package helm
	:ensure t
	:init
	(progn
		(defun helm-to-grep ()
			(helm-grep-mode)
			)

		(defun my/helm-buffer ()
			(interactive)
			(if (eq (projectile-project-root) nil)
					(helm-mini)
				(helm-projectile-switch-to-buffer)))
		
		(setq helm-follow-mode-persistent t)
		(general-define-key
		 :keymaps 'helm-map
		 "<tab>" 'helm-execute-persistent-action
		 "C-i" 'helm-execute-persistent-action
		 "C-z"  'helm-select-action
		 "<escape>" 'helm-keyboard-quit
		 "C-w" 'evil-delete-backward-word
		 "C-S-g" 'helm-end-of-buffer
		 "C-S-n" 'helm-next-source
		 "C-S-p" 'helm-previous-source
		 )
		(general-define-key
		 "M-x" 'helm-M-x
		 "C-x C-f" 'helm-find-files
		 "C-c q" 'helm-show-kill-ring
		 "C-x b" 'switch-to-buffer)
		(mikus-leader
			"i" 'helm-imenu
			"o" 'helm-occur
			"b" 'my/helm-buffer
			"B" 'helm-mini
			"f" 'helm-find-files
			"q" 'helm-show-kill-ring
			)
		(general-define-key
		 :keymaps '(helm-moccur-mode-map helm-grep-mode-map)
		 :states '(motion normal visual)
		 "C-c C-p" 'wgrep-change-to-wgrep-mode
		 )
		(helm-adaptive-mode)
		)
	:config
	;; Some nice keybindings

	;;(define-key helm-map (kbd "C-s") 'isearch-forward)

	(setq
	 helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
	 helm-split-window-inside-p t ; open helm buffer inside current window, not occupy whole other window
	 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
	 helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
	 helm-ff-file-name-history-use-recentf t
	 helm-echo-input-in-header-line t
	 helm-allow-mouse t)
	(setq helm-autoresize-max-height 0)
	(setq helm-autoresize-min-height 40)
	(helm-autoresize-mode 1)
	(setq helm-buffers-fuzzy-matching t
				helm-recentf-fuzzy-match    t)

	(evil-set-initial-state 'helm-mode 'emacs)
	(helm-mode 1)
	)

(use-package helm-youtube
	:after helm
	:ensure t
	:init
	(progn
		(setq browse-url-browser-function 'browse-url-generic)
		(setq browse-url-generic-program "chromium")
		(mikus-leader "y" 'helm-youtube)
		)
	)

(use-package helm-xref
	:after helm
	:ensure t
	:init
	(progn
		(require 'helm-xref)
		(setq xref-show-xrefs-function 'helm-xref-show-xrefs) ))

(defun spacemacs//helm-hide-minibuffer-maybe ()
	"Hide minibuffer in Helm session if we use the header line as input field."
	(when (with-helm-buffer helm-echo-input-in-header-line)
		(let ((ov (make-overlay (point-min) (point-max) nil nil t)))
			(overlay-put ov 'window (selected-window))
			(overlay-put ov 'face
									 (let ((bg-color (face-background 'default nil)))
										 `(:background ,bg-color :foreground ,bg-color)))
			(setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
					'spacemacs//helm-hide-minibuffer-maybe)






(provide 'init-helm)
