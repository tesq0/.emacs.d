(use-package company
  :hook (prog-mode . company-mode)
  :diminish "COMP"
  :config (progn

	    (require 'company-dabbrev)
	    (require 'company-dabbrev-code)
	    (require 'company-etags)
	    (require 'company-capf)

	    (defvar company-manual-completion-fn nil
	      "value of type FUNCTION, the function to use by company-manual-complete")

	    (defun company-manual-complete ()
	      (interactive)
	      (if (commandp company-manual-completion-fn)
		  (progn
		    (company-abort)
		    (call-interactively company-manual-completion-fn))
		(company-complete)
		))
	    
	    (defun company-quit ()
	      (interactive)
	      (company-abort)
	      (evil-keyboard-quit))

	    (general-define-key
	     :keymaps '(insert company-active-map)
	     "C-SPC" 'company-manual-complete
	     )

	    (general-define-key
	     :keymaps 'company-active-map
	     [tab] 'company-complete
	     "C-n" 'company-select-next
	     "C-p" 'company-select-previous
	     "C-w" 'evil-delete-backward-word
	     "C-e" 'company-complete-selection
	     "RET" 'company-complete-selection
	     "ESC" 'company-quit
	     "<escape>" 'company-quit)

	    ;; https://github.com/expez/company-quickhelp/issues/17

	    (add-hook 'company-completion-started-hook 'ans/set-company-maps)
	    (add-hook 'company-completion-finished-hook 'ans/unset-company-maps)
	    (add-hook 'company-completion-cancelled-hook 'ans/unset-company-maps)

	    (defun ans/unset-company-maps (&rest unused)
	      "Set default mappings (outside of company). Arguments (UNUSED) are ignored."
	      (general-def
		:states 'insert
		:keymaps 'override
		"C-n" nil
		"ESC" nil
		"<escape>" nil
		"C-p" nil))

	    (defun ans/set-company-maps (&rest unused)
	      "Set maps for when you're inside company completion. Arguments (UNUSED) are ignored."
	      (general-def
		:states 'insert
		:keymaps 'override
		"C-n" 'company-select-next
		"ESC" 'company-quit
		"<escape>" 'company-quit
		"C-p" 'company-select-previous))

	    (setq
		  ;; make previous/next selection in the popup cycles
		  company-selection-wrap-around t

		  company-dabbrev-ignore-case nil
		  company-dabbrev-downcase nil
		  company-dabbrev-code-ignore-case t
		  company-dabbrev-code-everywhere t
		  company-dabbrev-code-other-buffers 'nil
		  company-etags-ignore-case t

		  ;; Don't print anything to the echo area
		  company-echo-delay nil

		  ;; press M-number to choose candidate
		  company-show-numbers t

		  ;; When candidates in the autocompletion tooltip have additional
		  ;; metadata, like a type signature, align that information to the
		  ;; right-hand side. This usually makes it look neater.
		  company-tooltip-align-annotations t

		  company-idle-delay 0.2
		  company-minimum-prefix-length 1
		  company-require-match nil
		  )

	    
	    (setq company-backends '((:sorted company-files company-dabbrev-code company-capf company-yasnippet :with company-keywords)))
	    
	    (setq company-transformers '(company-sort-by-backend-importance company-sort-prefer-same-case-prefix))

	    ;; press SPACE will accept the highlighted candidate and insert a space
	    ;; `M-x describe-variable company-auto-complete-chars` for details
	    ;; That's BAD idea.
	    (setq company-auto-complete nil)

	    ;; NOT to load company-mode for certain major modes.
	    ;; Ironic that I suggested this feature but I totally forgot it
	    ;; until two years later.
	    ;; https://github.com/company-mode/company-mode/issues/29
	    (setq company-global-modes
		  '(not
		    eshell-mode comint-mode erc-mode gud-mode rcirc-mode
		    minibuffer-inactive-mode))
	    
	    (global-company-mode)))

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

(use-package helm-company
  :after company
  :commands (helm-company)
  :bind (:map company-active-map
	 ("C-s" . helm-company)))

(provide 'init-company)
