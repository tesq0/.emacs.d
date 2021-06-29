;;;  Mikus-Web-Config --- configuration for javascript with flycheck and shit


;;; Commentary:

;;; Code:

(use-package emmet-mode
  :ensure t)

(use-package prettier
  :ensure t)

(use-package add-node-modules-path
  :ensure t
  :hook ((typescript-mode . add-node-modules-path)
	  (web-mode . add-node-modules-path)))

(defun setup-typescript-tide-linter ()
  "Typescript tide backend setup."

  ;; (flycheck-select-checker 'javascript-tide)

  (if (not (setup-prettier))
      (add-hook 'before-save-hook 'tide-format-before-save)))

(defun setup-typescript-lsp-linter ()
  "Typescript lsp backend setup."

  (setq lsp-eslint-validate ["javascript" "javascriptreact" "typescript" "typescriptreact" "html"])
  (setq lsp-eslint-server-command 
	`("node" 
	  ;; ,(expand-file-name "~/.vscode/extensions/dbaeumer.vscode-eslint-2.1.14/server/out/eslintServer.js") 
	  ;; install version "2.0.11"
	  ,(expand-file-name "~/.emacs.d/server/vscode-eslint/server/out/eslintServer.js") 
	  "--stdio"))
  (lsp)
  (flycheck-select-checker 'javascript-eslint))

(defun setup-typescript-mode ()
  "Typescript setup."

  (tide-setup)
  (tide-hl-identifier-mode +1)
  (setq-local company-backends '((company-yasnippet company-tide company-files company-dabbrev-code company-keywords)))
  (setq-local company-manual-completion-fn #'company-tide)
  (setq-local emmet-expand-jsx-className? t)

  (eldoc-mode +1)
  (electric-pair-mode 1)
  (yas-minor-mode)
  (yas-reload-all)
  (emmet-mode)

  ;; (if (find-filename-in-project ".eslintrc.js")
  ;;     (setup-typescript-lsp-linter)
  ;;   (setup-typescript-tide-linter))
  )

(defun setup-prettier ()
  "Enable prettier-mode if it's configured."
  (when (find-filename-in-project ".prettierrc")
    (prettier-mode)))

(use-package nvm
  :ensure t)

(use-package tide
  :ensure t
  :init
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil :indentSize 2 :tabSize 2)
	tide-format-before-save nil)

  ;; (after-load
      ;; (flycheck-add-mode 'javascript-tide 'web-mode)
    ;; )
  )

(after-load 'typescript-mode
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'setup-typescript-mode))

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
	'(("jsx" . "\\.[jt]s[x]?\\'")
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
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

  (after-load 'flycheck
    (flycheck-add-mode 'css-csslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode))

  (defun setup-webmode ()
    "Does some setup depending on the current file extension."
    (electric-pair-local-mode 1)

    (let ((file-extension (file-name-extension buffer-file-name)))

      (when (or
	     (string-equal "php" file-extension)
	     (string-match "html?" file-extension)
	     (string-equal "htm" file-extension)
	     (string-equal "twig" file-extension)
	     (string-match "[jt]sx" file-extension))
	(emmet-mode))

      (when (string-match "[t]sx?" file-extension)
	(setup-prettier))

      (when (string-match "[jt]sx" file-extension)
	(setup-typescript-mode))

      (when (string-equal "s?css" file-extension)
	(setq-local flycheck-disabled-checkers '( javascript-eslint ))
	(rainbow-mode)
	(add-to-list (make-local-variable 'company-backends)
		     'company-css))))

  (add-hook 'web-mode-hook 'setup-webmode))

(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" .  js2-mode))
  (setq js-indent-level 2)
  (add-hook 'js2-mode-hook #'setup-typescript-mode))



(provide 'init-webmode)
;;; init-webmode.el ends here
