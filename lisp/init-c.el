;; C,C++

(defgroup c-options nil
  "Options for C language setup"
  :group 'init)

(defcustom c-langserver-disabled-modes '()
	"List of c langserver disabled modes."
	:group 'c-options
	:type '(set symbol))

(after-load 'lsp-mode
	(defun enable-irony-mode ()
		(and
		 (not (memq major-mode c-langserver-disabled-modes))
		 (lsp)))

	(add-hook 'c++-mode-hook 'irony-mode)
	(add-hook 'c-mode-hook 'enable-irony-mode)
	(add-hook 'objc-mode-hook 'irony-mode))

(defun init-c-style()
	"Define my own indenting style for C."
	(c-add-style "mikus-c"
							 '("gnu"
								 (c-basic-offset . 2)
								 (c-offsets-alist
									(innamespace . +)
									(template-args-cont . +)
									(inline-open . 0)
									(case-label . +)
									(inher-cont . c-lineup-multi-inher)
									(arglist-cont-nonempty . 0)
									(arglist-intro . +)
									(arglist-close . c-lineup-close-paren))))
	(add-to-list 'c-default-style '(c-mode . "mikus-c")))

(after-load 'cc-mode
	(init-c-style))

(provide 'init-c)
