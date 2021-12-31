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


(defvar jest-new-file nil)

(defun jest-test-after-find-file ()
  (and
   jest-new-file
   (yas-expand-snippet
    (yas-lookup-snippet "jestDescribe" 'typescript-mode)))
  (setq jest-new-file nil))

(defun goto-create-jest-test-for-file (filename)
  (let* ((test-filename
	  (format "%s.spec.%s"
		  (file-basename filename) (file-name-extension filename)))
	 (test-filepath (expand-file-name (format "__tests__/%s" test-filename)
					  (file-name-directory filename)))
	 (test-file-exits (file-exists-p test-filepath)))
    (when (not test-file-exits)
      (setq jest-new-file test-filepath))
    (find-file test-filepath)))

(defun goto-create-jest-test-for-current-file ()
  (interactive)
  (goto-create-jest-test-for-file (buffer-file-name)))

(define-prefix-command 'js-test-prefix-map)
(define-key 'js-test-prefix-map "j" 'jest-test-current-file)
(define-key 'js-test-prefix-map "n" 'nightwatch-test-current-file)
(define-key 'js-test-prefix-map "g" 'goto-create-jest-test-for-current-file)

(define-minor-mode js-test-minor-mode
  "Javascript testing mode."

  :lighter "js-test"
  (if js-test-minor-mode
      (progn
	(local-set-key (kbd "C-x t") 'js-test-prefix-map)
	(add-hook 'find-file-hook 'jest-test-after-find-file))
    (remove-hook 'find-file-hook 'jest-test-after-find-file)))

(defun setup-js-intellisense ()
  "JS intellisense."
  (js-test-minor-mode 1))

(use-package typescript-mode
  :hook (typescript-mode . setup-js-intellisense)
  :mode (("\\.js\\'" . typescript-mode)
	 ("\\.ts\\'" . typescript-mode))
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
	 ("\\.htm[l]?\\'" . web-mode)
	 ("\\.twig\\'" . web-mode)
	 ("\\.svelte\\'" . web-mode)
	 ("\\.ftl\\'" . web-mode))
  :init
  (defun setup-webmode ()
    "Does some setup depending on the current file extension."

    (when-file-extension-matches
     '("[jt]sx" "svelte")
     'setup-js-intellisense))

  (add-hook 'web-mode-hook 'setup-webmode)

  :config

  (general-unbind
    :keymaps 'web-mode-map
    "C-c C-s")

  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset nil
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

  ;; (with-eval-after-load 'flycheck
  ;;   (flycheck-add-mode 'css-csslint 'web-mode)
  ;;   (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;;   )
  )

(use-package rainbow-mode
  :hook (web-mode . (lambda () (when-file-extension-matches "[s]?css" 'rainbow-mode))))

(provide 'init-webmode)
;;; init-webmode.el ends here
