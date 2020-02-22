;;;  Mikus-Web-Config --- configuration for javascript with flycheck and shit


;;; Commentary:

;;; Code:

(define-prefix-command 'javascript-prefix)

(defun setup-tide-mode ()
	(interactive)
	(tide-setup)
	(eldoc-mode +1)
	(tide-hl-identifier-mode +1)
	(electric-pair-mode 1)
	(yas-minor-mode)
	(yas-reload-all)
	(setq-local company-backends '((company-yasnippet company-tide company-files company-dabbrev-code company-keywords)))
	(setq-local company-manual-completion-fn #'company-tide)
		;; formats the buffer before saving
	(prettier-mode)
	;;(add-hook 'before-save-hook 'tide-format-before-save)

	)

(use-package iter2
	:ensure t)

(use-package nvm
	:ensure t)

(use-package tide
	:ensure t
	:init
	(add-hook 'typescript-mode-hook 'setup-tide-mode)
	:config
	(setq typescript-indent-level 2)
	(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :indentSize 2 :tabSize 2)
				tide-format-before-save nil)
	)

(after-load 'prettier
	(add-to-list 'prettier-major-mode-parsers '(web-mode typescript))
	(setq prettier-inline-errors-flag t)
	)


(use-package indium
	:ensure t)

(defun setup-json()
	(setq json-reformat:indent-width 1)
	)

(use-package json-mode
	:ensure t
	:init
	(setup-json))

(after-load 'flycheck
	(flycheck-add-mode 'css-csslint 'web-mode)
	(flycheck-add-mode 'typescript-tslint 'web-mode)
	(flycheck-add-mode 'javascript-eslint 'web-mode))


(use-package web-mode
	:ensure t
	:init
	;; adjusting indentation level
	(setq web-mode-markup-indent-offset 4
				web-mode-css-indent-offset 4
				web-mode-code-indent-offset 4
				web-mode-enable-auto-quoting nil
				js-indent-level 2)
	:config
	(setq web-mode-auto-quote-style nil)
	;;(define-key web-mode-map (kbd "C-t") (lookup-key web-mode-map (kbd "C-c C-t")))
	)

(use-package tern
	:ensure t
	:config
	(define-key tern-mode-keymap (kbd "M-.") nil)
	(define-key tern-mode-keymap (kbd "M-,") nil)
	)


(use-package js2-mode
	:ensure t
	)

(use-package js2-refactor
	:ensure t
	)

;; javascript interpreter in a window
(use-package js-comint
	:ensure t)

(use-package emmet-mode
	:ensure t
	:hook (web-mode . emmet-mode))

;; use web-mode for js,jsx and css files
(add-to-list 'auto-mode-alist '("\\.js\\'" .  web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))



(add-hook 'js2-mode-hook
					(lambda ()
						;; selecting flycheck checkers based on file modes
						(when (string-equal "js" (file-name-extension buffer-file-name))
							(setq-local flycheck-disabled-checkers '( css-csslint ))
							;; (setup-tide-mode)
							;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
							;; (flycheck-add-next-checker 'javascript-eslint 'tsx-tide 'append)
							;; (flycheck-add-next-checker 'javascript-tide 'jsx-tide 'append)
							(js2-refactor-mode)
							(js2r-add-keybindings-with-prefix "C-c C-r")
							(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
							(define-key js-mode-map (kbd "M-.") nil)
							(add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
							(tern-mode t)

							(company-mode)
							)
						)

					)



(defun setup-webmode ()
	"Does some setup depending on the current file extension."
	(electric-pair-local-mode 1)

	(let ((file-extension (file-name-extension buffer-file-name)))

		(when (string-equal "tsx" file-extension)
			(setup-tide-mode))

		(when (string-equal "css" file-extension)
			(setq-local flycheck-disabled-checkers '( javascript-eslint ))
			(rainbow-mode)
			(add-to-list (make-local-variable 'company-backends)
									 'company-css))

		(when (string-equal "js" file-extension)
			(setq-local flycheck-disabled-checkers '( css-csslint ))
			;; (color-theme-buffer-local 'color-theme-sanityinc-tomorrow-night (current-buffer))
			;; (setup-tide-mode)
			(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
			(flycheck-add-next-checker 'javascript-eslint 'tsx-tide 'append)
			;; (flycheck-add-next-checker 'javascript-tide 'jsx-tide 'append)
			(add-to-list (make-local-variable 'company-backends)
									 'company-tern)
			(tern-mode t)
			(prettier-mode)
			(local-set-key (kbd "C-j") 'javascript-prefix)
			(company-mode))
		)
	)

(add-hook 'web-mode-hook 'setup-webmode)

;; use eslint with web-mode for jsx files

;; customize flycheck temp file prefix
;;(setq-default flycheck-temp-prefix ".flycheck")


;; enable web mode element highlighting
(setq web-mode-enable-current-element-highlight t)
;; auto pairing

;;(setq web-mode-enable-auto-pairing t)

;; css
;; (setq web-mode-enable-css-colorization t)


(setq web-mode-content-types-alist
			'(("jsx" . "\\.js[x]?\\'")
				("javascript" . "\\.es6?\\'")))



;; also use jsx for js
(defadvice web-mode-highlight-part (around tweak-jsx activate)
	(if (equal web-mode-content-type "jsx")
			(let ((web-mode-enable-part-face nil))
				ad-do-it)
				ad-do-it))





;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))
;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)






(provide 'init-webmode)
;;; mikus-webmode.el ends here
