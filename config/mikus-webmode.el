;;;  Mikus-Web-Config --- configuration for javascript with flycheck and shit


;;; Commentary:

;;; Code:


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  )

;; aligns annotation to the right hand side

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package tide
	:ensure t
	:config
	(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :indentSize 2 :tabSize 2))
	)

(use-package indium
	:ensure t
	) 

(use-package json-mode
	:ensure t
	)

(use-package flycheck
  :ensure t
	:config
	(setq flycheck-highlighting-mode 'lines)
	(setq flycheck-check-syntax-automatically '(mode-enabled save))
	(global-flycheck-mode)
	(flycheck-add-mode 'css-csslint 'web-mode)
	(flycheck-add-mode 'javascript-eslint 'web-mode)
)


(use-package web-mode
  :ensure t
	:init
	;; adjusting indentation level
	(setq web-mode-markup-indent-offset 2)
	(setq web-mode-css-indent-offset 2)
	(setq web-mode-code-indent-offset 2)
	)



(use-package js-doc
	:ensure t
	)


;; disable jshint, we will use eslint instead
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))


(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))


;; use web-mode for js,jsx and css files
(add-to-list 'auto-mode-alist '("\\.js\\'" .  web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-hook 'web-mode-hook
					(lambda () 
						;; selecting flycheck checkers based on file modes
						(when (string-equal "css" (file-name-extension buffer-file-name))
							(setq-local flycheck-disabled-checkers '( javascript-eslint ))) 
						(when (string-equal "js" (file-name-extension buffer-file-name))
							(setq-local flycheck-disabled-checkers '( css-csslint ))
							;; (setup-tide-mode)
							(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append) 
							(flycheck-add-next-checker 'javascript-eslint 'tsx-tide 'append) 
							;; (flycheck-add-next-checker 'javascript-tide 'jsx-tide 'append)
							(tern-mode t)
							(evil-leader/set-key
								"j" (lambda () (interactive) (evil-without-repeat (call-interactively #'hydra-jsdoc/body))))
							)
						)
					
					)


;; use eslint with web-mode for jsx files

;; customize flycheck temp file prefix
;;(setq-default flycheck-temp-prefix ".flycheck")


;; enable web mode element highlighting
(setq web-mode-enable-current-element-highlight t)
;; auto pairing

;;(setq web-mode-enable-auto-pairing t)

;; css
(setq web-mode-enable-css-colorization t)


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






(provide 'mikus-webmode)
;;; mikus-webmode.el ends here
