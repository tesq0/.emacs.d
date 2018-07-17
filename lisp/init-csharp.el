;;;  Mikus-Csharp-Config --- configuration for csharp


;;; Commentary:

;;; Code:

(defun initCsharp ()
	"Initate csharp speciifc stuff."
	(omnisharp-mode)
	(company-mode)
	(flycheck-mode)

	(setq-local yas-indent-line 'fixed)
	(yas-minor-mode)


	(c-add-style "mikus-csharp"
							 '("ellemtel"
								 (c-basic-offset . 4)
								 (c-offsets-alist
									(innamespace . -)
									(inline-open . 0)
									(inher-cont . c-lineup-multi-inher)
									(arglist-cont-nonempty . +)
									(arglist-intro . +)
									(arglist-close . c-lineup-close-paren)
									(template-args-cont . +))))

	(setq c-default-style "mikus-csharp")
	(setq-local tab-width 4)
	(setq c-syntactic-indentation 1)
	(setq indent-tabs-mode t)
	(c-set-style "mikus-csharp")
	(setq omnisharp-debug t)

	(cl-pushnew 'company-omnisharp company-backends)

	;; (electric-pair-local-mode 1)

	;; (electric-indent-local-mode -1)


	;; (local-set-key (kbd "M-.") 'omnisharp-go-to-definition)
	;; (local-set-key (kbd "M-,") 'omnisharp-))
	(local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
	(local-set-key (kbd "C-c C-c") 'recompile))


(use-package omnisharp
	:ensure t
	:init
	(progn
		(add-hook 'csharp-mode-hook 'initCsharp t)
		(general-define-key
		 :keymaps 'csharp-mode-map
	   "M-." 'omnisharp-go-to-definition
		 "C-c u" 'omnisharp-find-usages
		 "C-c i" 'omnisharp-find-implementations
		 ))
	)

(provide 'init-csharp)
;;; mikus-csharp.el ends here
