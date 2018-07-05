(use-package helm
	:ensure t
	:config

	(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
	(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
	(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
	(global-set-key (kbd "M-x") 'helm-M-x) ; list actions using C-z
	(define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)

	;; Some nice keybindings
	(define-key helm-map (kbd "C-g") 'helm-beginning-of-buffer)
	(define-key helm-map (kbd "C-S-g") 'helm-end-of-buffer)

	(define-key helm-map (kbd "C-S-n") 'helm-next-source)
	(define-key helm-map (kbd "C-S-p") 'helm-previous-source)

	;;(define-key helm-map (kbd "C-s") 'isearch-forward)

	(setq
	 helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
	 helm-split-window-inside-p t ; open helm buffer inside current window, not occupy whole other window
	 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
	 helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
	 helm-ff-file-name-history-use-recentf t
	 helm-echo-input-in-header-line t)
	(setq helm-autoresize-max-height 0)
	(setq helm-autoresize-min-height 40)
	(helm-autoresize-mode 1)
	(setq helm-buffers-fuzzy-matching t
				helm-recentf-fuzzy-match    t)

	(evil-set-initial-state 'helm-mode 'emacs)
	(helm-mode 1)
	)


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


(use-package helm-projectile
	:ensure t
	:diminish
	:init
	(projectile-mode)
	(helm-projectile-on)
	(helm-add-action-to-source "Oper deer ranger in project `M-r'" #'deer helm-source-projectile-projects)
	(define-key projectile-command-map (kbd "<ESC>") nil)
	(helm-projectile-define-key helm-projectile-projects-map (kbd "M-r") #'deer))
(use-package evil-visualstar
	:ensure t)


(provide 'init-helm)