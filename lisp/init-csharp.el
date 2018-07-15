;;;  Mikus-Csharp-Config --- configuration for csharp


;;; Commentary:

;;; Code:

(defun initCsharp ()
	"Initate csharp speciifc stuff."
	(omnisharp-mode)
	(company-mode)
	(flycheck-mode)

	(setq indent-tabs-mode nil)
	(setq c-syntactic-indentation t)
	(c-set-style "ellemtel")
	(setq c-basic-offset 4)
	(setq truncate-lines t)
	(setq tab-width 4)
	(setq evil-shift-width 4)
	(setq omnisharp-server-executable-path (expand-file-name "~/.emacs.d/omnisharp/start-omnisharp.sh"))
	;; (setq omnisharp-server-executable-path "C:\\Bin\\omnisharp-roslyn\\OmniSharp.exe")
	(setq omnisharp-debug t)



	(general-define-key
	 :keymaps 'csharp-mode-map
	 "M-." 'omnisharp-go-to-definition)
	;; (local-set-key (kbd "M-.") 'omnisharp-go-to-definition)
	;; (local-set-key (kbd "M-,") 'omnisharp-))
	(local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
	(local-set-key (kbd "C-c C-c") 'recompile))


(use-package omnisharp
	:ensure t
	:config 
	(add-hook 'csharp-mode-hook 'initCsharp t)
	)

(provide 'init-csharp)
;;; mikus-csharp.el ends here
