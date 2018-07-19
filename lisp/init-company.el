(use-package company
	:init (progn
					(add-hook 'prog-mode-hook 'company-mode))
	:diminish "COMP"
	:config (progn
						(define-key company-active-map [tab] 'company-complete)
						(define-key company-active-map (kbd "C-n") 'company-select-next)
						(define-key company-active-map (kbd "C-p") 'company-select-previous)

						(setq company-dabbrev-downcase nil
									;; make previous/next selection in the popup cycles
									company-selection-wrap-around t
									;; Some languages use camel case naming convention,
									;; so company should be case sensitive.
									company-dabbrev-ignore-case nil
									;; press M-number to choose candidate
									company-show-numbers t
									company-idle-delay nil
									company-clang-insert-arguments nil
									company-require-match nil
									company-etags-ignore-case t)


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


(require 'company-etags)
(after-load 'company-etags
	(progn
		 (add-to-list 'company-etags-modes 'csharp-mode)
		 (add-to-list 'company-etags-modes 'web-mode)
		 (add-to-list 'company-etags-modes 'lua-mode)))

(provide 'init-company)
