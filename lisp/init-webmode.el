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
  :hook (web-mode . maybe-emmet-mode)
  :config
  (setq emmet-expand-jsx-className? t))

(use-package prettier
  :commands (prettier-prettify)
  :bind (("C-c f" . prettier-prettify)
	 ("C-x f" . prettier-prettify))
  :init
  (defun setup-prettier ()
    "Enable prettier-mode if it's configured."
    (when (find-filename-in-project ".prettierrc")
      (prettier-mode))))

(use-package add-node-modules-path
  :hook ((typescript-mode . add-node-modules-path)
	  (web-mode . add-node-modules-path)))


(defgroup nightwatch nil
  "Nightwatch."
  :group 'root)

(defcustom nightwatch-test-cmd "npx nw -t %s --"
  "Command to use for running a nightwatch test."
  :type 'string
  :group 'nightwatch)

(defgroup javascript nil
  "Javascript."
  :group 'root)

(defcustom js-localization-function "t"
  "Localization function."
  :type 'string
  :group 'javascript)

(defun nightwatch-test-current-file ()
  "Run a nightwatch test defined in the current file."
  (interactive)
  (compile (format nightwatch-test-cmd (buffer-file-name))))

(defun jest-test-current-file ()
  "Run a jest test defined in the current file."
  (interactive)
  (compile (format "npx jest %s" (file-basename (buffer-name)))))

(define-prefix-command 'js-test-prefix-map)
(define-key 'js-test-prefix-map "j" 'jest-test-current-file)
(define-key 'js-test-prefix-map "n" 'nightwatch-test-current-file)


(defun setup-js-intellisense ()
  "JS intellisense."
  (local-set-key (kbd "C-x t") 'js-test-prefix-map)
  (eldoc-mode +1))

(use-package typescript-mode
  :hook (typescript-mode . setup-js-intellisense)
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
	 ("\\.[s]?css\\'" . web-mode)
	 ("\\.cshtml\\'" . web-mode)
	 ("\\.blade.php\\'" . web-mode)
	 ("\\.htm\\'" . web-mode)
	 ("\\.twig\\'" . web-mode)
	 ("\\.svelte\\'" . web-mode)
	 ("\\.ftl\\'" . web-mode))
  :init
  (defun setup-webmode ()
    "Does some setup depending on the current file extension."

    (when-file-extension-matches
     '("[jt]sx" "svelte")
     'setup-js-intellisense)

    (let ((file-extension (file-name-extension buffer-file-name)))
      (when (string-equal "s?css" file-extension)
	(setq-local flycheck-disabled-checkers '( javascript-eslint ))
	(add-to-list (make-local-variable 'company-backends)
		     'company-css))))

  (add-hook 'web-mode-hook 'setup-webmode)

  :config

  (general-unbind
    :keymaps 'web-mode-map
    "C-c C-s")

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
  :config
  (setq js-indent-level 2))


(use-package rainbow-mode
  :mode ("\\.[s]css\\'" . rainbow-mode))

(provide 'init-webmode)
;;; init-webmode.el ends here
