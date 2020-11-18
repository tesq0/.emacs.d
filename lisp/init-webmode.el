;;;  Mikus-Web-Config --- configuration for javascript with flycheck and shit


;;; Commentary:

;;; Code:

(use-package emmet-mode
  :ensure t)

(defun setup-tide-mode ()
  "Typescript mode setup."
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (electric-pair-mode 1)
  (yas-minor-mode)
  (yas-reload-all)
  (setq-local company-backends '((company-yasnippet company-tide company-files company-dabbrev-code company-keywords)))
  (setq-local company-manual-completion-fn #'company-tide)
  ;; formats the buffer before saving
  (emmet-mode)
  ;; (prettier-mode)
  (add-hook 'before-save-hook 'tide-format-before-save))

(use-package nvm
  :ensure t)

(use-package tide
  :ensure t
  :init
  (setq typescript-indent-level 2)
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :indentSize 2 :tabSize 2)
	tide-format-before-save nil)
  (add-hook 'typescript-mode-hook 'setup-tide-mode))

(use-package json-mode
  :ensure t
  :init
  (defun setup-json()
    (setq json-reformat:indent-width 1))
  (setup-json))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 4
	web-mode-code-indent-offset 2
	web-mode-enable-auto-quoting nil
	web-mode-enable-current-element-highlight t
	web-mode-auto-quote-style nil)

  (setq web-mode-engines-alist
	'(("php"    . "\\.htm\\'")))

  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'")
	  ("javascript" . "\\.es6?\\'")))

  ;; also use jsx for js
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
	(let ((web-mode-enable-part-face nil))
	  ad-do-it)
      ad-do-it))

  ;; use web-mode for js,jsx and css files
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))

  (flycheck-add-mode 'css-csslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  
  (add-hook 'web-mode-hook 'setup-webmode))


(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" .  js2-mode))
  (add-hook 'js2-mode-hook
	    (lambda ()
	      (setup-tide-mode))))



(provide 'init-webmode)
;;; mikus-webmode.el ends here
