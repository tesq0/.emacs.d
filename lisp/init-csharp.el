;;;  Mikus-Csharp-Config --- configuration for csharp


;;; Commentary:

;;; Code:
(require 'ide-bridge)

(defvar ide-bridge-path "C:/Sources/idebridge/bin/IdeBridge.exe")

(defun start-IDE-bridge ()
	(interactive)
	(async-shell-command ide-bridge-path)
	)

(defun initCsharp ()
	"Initate csharp speciifc stuff."
	(omnisharp-mode)

	;; (ide-bridge-setup)

	(flycheck-mode)

	(setq-local yas-indent-line 'fixed)
	(yas-minor-mode)
	(yas-reload-all)

	(c-add-style "mikus-csharp"
							 '("c#"
								 (c-basic-offset . 4)
								 (c-offsets-alist
									(innamespace . -)
									(inline-open . 0)
									(inher-cont . c-lineup-multi-inher)
									(arglist-cont-nonempty . 0)
									(arglist-intro . +)
									(arglist-close . c-lineup-close-paren)
									(template-args-cont . +))))

	(setq c-default-style "mikus-csharp")
	(setq-local tab-width 4)
	(setq c-syntactic-indentation 1)
	(setq indent-tabs-mode nil)
	(c-set-style "mikus-csharp")
	;; (setq omnisharp-debug t)

	(cl-pushnew 'company-omnisharp company-backends)

  
	(local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
	(local-set-key (kbd "C-c C-c") 'recompile))




(use-package omnisharp
	:ensure t
	:init
	(progn
		(add-hook 'csharp-mode-hook 'initCsharp t)
		(setq omnisharp-imenu-support t)
		(setq omnisharp-eldoc-support nil)
		(setq omnisharp-auto-complete-want-documentation nil)
		;; (setq omnisharp-debug t)
		(setq omnisharp-server-executable-path "C:\\Users\\mikol\\AppData\\Roaming\\.emacs.d\\.cache\\omnisharp-win-x86\\OmniSharp.exe")

		(general-define-key
		 :keymaps 'csharp-mode-map
	   "M-." 'omnisharp-go-to-definition
	   "M->" 'omnisharp-go-to-definition-other-window
		 "C-c u" 'omnisharp-find-usages
		 "C-c i" 'omnisharp-find-implementations
		 ;; "C-SPC" 'ide-bridge-complete
		 )
		;; (general-define-key
		;;  :keymaps 'csharp-mode-map
		;;  :states 'insert
		;;  "C-n" 'ide-bridge-completion-next-line
		;;  "C-p" 'ide-bridge-completion-previous-line
		;;  )
		)
	)

(defvar csharp-org-langs (list "csharp" "cs"))

(defun csharp-check-fontiy (limit)
	"Check if in org source block, then check if the source block is csharp and if so set vars to use csharp highlighting by font-lock."
	(when (org-in-src-block-p)
		(let* ( (lang (car (org-babel-get-src-block-info 'light)))
						(csharp-p (member lang csharp-org-langs))
						(syntax-table (or (and csharp-p csharp-mode-syntax-table) nil))
						(syntax-function (or (and csharp-p #'csharp-mode-syntax-propertize-function) nil)) )

			(message (format "is in csharp block? %s" csharp-p))
			(setq-local font-lock-syntax-table syntax-table)
			(setq-local syntax-propertize-function syntax-function)
			)
		)
	)

;; (add-hook 'org-font-lock-hook 'csharp-check-fontiy)
;; (remove-hook 'org-font-lock-hook 'csharp-check-fontiy)

;; (advice-add 'jit-lock-function :before #'csharp-check-fontify)

;; (defun clear-jit-lock-advices ()
;; 	(interactive)
;; 	(let* ((name "jit-lock-function"))
;; 		(advice-mapc #'(lambda (advice props) (general-remove-advice (intern name) advice)) (intern name))
;; 		)
;; 	)





(provide 'init-csharp)
;;; mikus-csharp.el ends here
