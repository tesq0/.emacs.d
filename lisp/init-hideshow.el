
(use-package hideshow
	:ensure t
	:init
	(progn
	 (defvar hs-special-modes-alist
		 (mapcar 'purecopy
						 '((c-mode "{" "}" "/[*/]" nil nil)
							 (c++-mode "{" "}" "/[*/]" nil nil)
							 (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
							 (java-mode "{" "}" "/[*/]" nil nil)
							 (js-mode "{" "}" "/[*/]" nil))))

	 (add-hook 'c-mode-common-hook   'hs-minor-mode)
	 (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
	 (add-hook 'java-mode-hook       'hs-minor-mode)
	 (add-hook 'lisp-mode-hook       'hs-minor-mode)
	 (add-hook 'perl-mode-hook       'hs-minor-mode)
	 (add-hook 'sh-mode-hook         'hs-minor-mode)
	 (add-hook 'csharp-mode-hook      'hs-minor-mode)
	 (add-hook 'web-mode-hook         'hs-minor-mode)))

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
