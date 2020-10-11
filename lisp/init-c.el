;; C,C++

(defgroup c-options nil
  "Options for C language setup"
  :group 'init)

(defcustom c-langserv-disabled-modes '()
	"List of disabled modes (where langserver sound not start)."
	:group 'c-options
	:type '(set symbol))

(defun enable-c-langserver ()
	"Enable C langserver."
	(and
	 (not (memq major-mode c-langserv-disabled-modes))
	 (lsp)))

(add-hook 'c++-mode-hook 'enable-c-langserver)
(add-hook 'c-mode-hook 'enable-c-langserver)
(add-hook 'objc-mode-hook 'enable-c-langserver)

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
	(add-to-list 'c-default-style '(c-mode . "mikus-c"))
	)

(after-load 'cc-mode
	(init-c-style))

(provide 'init-c)
