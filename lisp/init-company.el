(use-package company
	:init (progn
					(add-hook 'prog-mode-hook 'company-mode))
	:diminish "COMP"
	:config (progn

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
							(evil-keyboard-quit)
							)

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
						 "ESC" 'company-quit
						 "<escape>" 'company-quit
						 )
						(setq company-dabbrev-downcase nil

									;; make previous/next selection in the popup cycles
									company-selection-wrap-around t

									;; Some languages use camel case naming convention,
									;; so company should be case sensitive.
									company-dabbrev-ignore-case t

									company-dabbrev-code-ignore-case t
									company-dabbrev-code-everywhere t
									company-etags-ignore-case t

									;; press M-number to choose candidate
									company-show-numbers t

									;; When candidates in the autocompletion tooltip have additional
									;; metadata, like a type signature, align that information to the
									;; right-hand side. This usually makes it look neater.
									company-tooltip-align-annotations t

									company-idle-delay 0.2
									company-minimum-prefix-length 1
									company-clang-insert-arguments nil
									company-require-match nil
									)
						(setq company-transformers '(company-sort-prefer-same-case-prefix))

						(defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
							;; you can use (ad-get-arg 0) and (ad-set-arg 0) to tweak the arguments
							(if (memq major-mode '(php-mode html-mode web-mode nxml-mode))
									(setq ad-return-value nil)
								ad-do-it))

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

						(add-hook 'org-mode-hook 'company-ispell-setup)

						(defun toggle-company-ispell ()
							(interactive)
							(cond
							 ((memq 'company-ispell company-backends)
								(setq company-backends (delete 'company-ispell company-backends))
								(message "company-ispell disabled"))
							 (t
								(add-to-list 'company-backends 'company-ispell)
								(message "company-ispell enabled!"))))

						(defun company-ispell-setup ()
							;; @see https://github.com/company-mode/company-mode/issues/50
							(when (boundp 'company-backends)
								(make-local-variable 'company-backends)
								(add-to-list 'company-backends 'company-ispell)
								;; https://github.com/redguardtoo/emacs.d/issues/473
								(if (and (boundp 'ispell-alternate-dictionary)
												 ispell-alternate-dictionary)
										(setq company-ispell-dictionary ispell-alternate-dictionary))))
						(global-company-mode)
						))

(use-package company-quickhelp
	:config
	(setq company-quickhelp-delay 1)
	(company-quickhelp-mode 1))


;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
	:demand t
	:after company
	:config

	;; Use `prescient' for Company menus.
	(company-prescient-mode +1))



;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.
(use-package eldoc
  :demand t
  :config

  ;; Always truncate ElDoc messages to one line. This prevents the
  ;; echo area from resizing itself unexpectedly when point is on a
  ;; variable with a multiline docstring.
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Original code from
  ;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
  ;; thanks!
  (radian-defadvice radian--advice-disable-eldoc-on-flycheck
      (&rest _)
    :after-while eldoc-display-message-no-interference-p
    "Disable ElDoc when point is on a Flycheck overlay.
This prevents ElDoc and Flycheck from fighting over the echo
area."
    (not (and (bound-and-true-p flycheck-mode)
              (flycheck-overlay-errors-at (point)))))

)

;; Add a completion source for emoji. 😸

(use-package company-emoji
	:config
	(company-emoji-init))

(require 'company-etags)
(after-load 'company-etags
	(progn
		 (add-to-list 'company-etags-modes 'csharp-mode)
		 (add-to-list 'company-etags-modes 'web-mode)
		 (add-to-list 'company-etags-modes 'lua-mode)))

(provide 'init-company)
