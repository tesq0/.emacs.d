;;;  Mikus-Web-Config --- configuration for javascript with flycheck and shit


;;; Commentary:

;;; Code:

(use-package emmet-mode
  :init
  (defun maybe-emmet-mode ()
    (when-file-extension-matches
     '("php"
       "html?"
       "htm"
       "twig"
       "svelte"
       "[jt]sx") 'emmet-mode))
  :hook (web-mode . maybe-emmet-mode))

(use-package prettier
  :init
  (defun setup-prettier ()
    "Enable prettier-mode if it's configured."
    (when (find-filename-in-project ".prettierrc")
      (prettier-mode))))

(use-package add-node-modules-path
  :hook ((typescript-mode . add-node-modules-path)
	  (web-mode . add-node-modules-path)))

(use-package nvm)

(use-package tide
  :after web-mode
  :commands (tide-setup)
  :init
  (defun setup-tide-mode ()
    "Typescript setup."
    (tide-setup)
    (tide-hl-identifier-mode +1)
    (setq-local company-backends '((company-yasnippet company-tide company-files company-dabbrev-code company-keywords)))
    (setq-local company-manual-completion-fn #'company-tide)
    (setq-local emmet-expand-jsx-className? t))

  (defun maybe-tide-mode ()
    "Setup tide mode when the current file extension matches"
    (when-file-extension-matches
     '("[jt]sx" "svelte")
     'setup-tide-mode))

  :hook ((typescript-mode . setup-tide-mode)
	 (web-mode . maybe-tide-mode))
  :config
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :indentSize 2 :tabSize 2)
	tide-format-before-save nil))

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package json-mode
  :after web-mode
  :mode ("\\.json\\'" . json-mode)
  :config
  (setq json-reformat:indent-width 1))

(use-package web-mode
  :mode (("\\.jsx\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.vue\\'" . web-mode)
	 ("\\.css\\'" . web-mode)
	 ("\\.cshtml\\'" . web-mode)
	 ("\\.htm\\'" . web-mode)
	 ("\\.twig\\'" . web-mode)
	 ("\\.svelte\\'" . web-mode)
	 ("\\.ftl\\'" . web-mode))
  :init
  (defun setup-webmode ()
    "Does some setup depending on the current file extension."

    ;; (turn-on-eldoc-mode)

    (let ((file-extension (file-name-extension buffer-file-name)))
      (when (string-equal "s?css" file-extension)
	(setq-local flycheck-disabled-checkers '( javascript-eslint ))
	(add-to-list (make-local-variable 'company-backends)
		     'company-css))))

  (add-hook 'web-mode-hook 'setup-webmode)

  :config
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 4
	web-mode-code-indent-offset 2
	web-mode-enable-auto-quoting nil
	web-mode-enable-current-element-highlight t
	web-mode-auto-quote-style nil)
  
  (setq web-mode-engines-alist
	'(("php"    . "\\.htm\\'")))

  (setq web-mode-content-types-alist
	'(("jsx" . "\\.[jt]s[x]?\\'")
	  ("javascript" . "\\.es6?\\'")))

  ;; also use jsx for js
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
	(let ((web-mode-enable-part-face nil))
	  ad-do-it)
      ad-do-it))

  ;; use web-mode for js,jsx and css files

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'css-csslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)))

(use-package js2-mode
  :mode ("\\.js\\'" .  js2-mode)
  :hook (js2-mode . setup-typescript-mode)
  :config
  (setq js-indent-level 2))


(use-package rainbow-mode
  :mode ("\\.[s]css\\'" . rainbow-mode))

(provide 'init-webmode)
;;; init-webmode.el ends here
