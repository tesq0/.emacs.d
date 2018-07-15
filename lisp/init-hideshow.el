
(use-package hideshow
	:ensure t
	:config
	(defvar hs-special-modes-alist
	(mapcar 'purecopy
	'((c-mode "{" "}" "/[*/]" nil nil)
		(c++-mode "{" "}" "/[*/]" nil nil)
		(bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
		(java-mode "{" "}" "/[*/]" nil nil)
		(js-mode "{" "}" "/[*/]" nil))))

	(define-prefix-command	'fold-prefix)

	(evil-leader/set-key "/" 'toggle-hiding)
	(add-hook 'c-mode-common-hook   'hs-minor-mode)
	(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
	(add-hook 'java-mode-hook       'hs-minor-mode)
	(add-hook 'lisp-mode-hook       'hs-minor-mode)
	(add-hook 'perl-mode-hook       'hs-minor-mode)
	(add-hook 'sh-mode-hook         'hs-minor-mode)
	(add-hook 'csharp-mode-hook      'hs-minor-mode)
	(add-hook 'web-mode-hook         'hs-minor-mode)
	(evil-leader/set-key "2" 'fold-prefix)

	(define-key fold-prefix (kbd "t") 'toggle-hiding)
	(define-key fold-prefix (kbd "s") 'hs-show-block)
	(define-key fold-prefix (kbd "h") 'hs-hide-block)
	(define-key fold-prefix (kbd "S") 'hs-show-all)
	(define-key fold-prefix (kbd "H") 'hs-hide-all)
	(define-key fold-prefix (kbd "l") 'hs-hide-level)
	(define-key fold-prefix (kbd "L") 'hs-hide-level-recursive)


	)

(defun toggle-selective-display (column)
	(interactive "P")
	(set-selective-display
	 (or column
			 (unless selective-display
				 (1+ (current-column))))))

(defun toggle-hiding (column)
	(interactive "P")
	(if hs-minor-mode
			(if (condition-case nil
							(hs-toggle-hiding)
						(error t))
					(hs-show-all))
		(toggle-selective-display column)))

(provide 'init-hideshow)
