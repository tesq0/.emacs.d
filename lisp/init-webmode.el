;;;  Mikus-Web-Config --- configuration for javascript with flycheck and shit


;;; Commentary:

;;; Code:

(autoload 'emmet-mode "emmet-mode")

(defun maybe-emmet-mode ()
    (when-file-extension-matches
     '("php"
       "html?"
       "htm"
       "twig"
       "svelte"
       "[jt]sx") 'emmet-mode))

(with-eval-after-load 'emmet-mode
  (setq emmet-expand-jsx-className? t))

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
  (let ((default-directory (or (projectile-project-root) default-directory)))
   (compile (format "npx jest %s" (buffer-file-name)))))

(defvar jest-new-file nil)

(defun parse-js-exports (filename)
  (let ((json (shell-command-to-string
    (format "deno run --allow-read %s %s"
	    "/Users/mikolaj.galkowski/Projects/js-langserver/get-exports.ts"
	    filename))))
    (message json)
  (json-parse-string json
   :array-type 'list)))

(defmacro define-js-module-import-command (command doc &optional find-file-strategy)
  `(defun ,command (filename exported-variable)
     ,doc
     (interactive (let* ((fn
			  (or ,find-file-strategy
			      (read-file-name "Find file: " nil default-directory (confirm-nonexistent-file-or-buffer))))
			 (exports (parse-js-exports fn)))
			 (list fn
			       (if (length= exports 0) (file-basename fn)
				 (format "{%s}" (if (length= exports 1) (car exports)
						 (completing-read "Choose import" exports)))))))
     (insert (format "import %s from \"%s\";" exported-variable
		     (let ((index-file (expand-file-name "index.ts" (file-name-directory filename))))
		       (if (and
			    (file-exists-p index-file)
			    (with-temp-buffer
			      (insert-file-contents index-file)
			      (condition-case nil (re-search-forward (format "export {%s} from.*" (file-basename filename)))
				(error nil)
				(:success t))))
			   (string-trim-right (my/file-relative-name (file-name-directory filename)) "\/")
			 (file-path-no-extension (my/file-relative-name filename))))))))

(define-js-module-import-command js-module-relative-import "Import a js module relatively.")
(define-js-module-import-command js-module-project-import "Import a js module in project."
  (if (fboundp 'projectile-completing-read-file) (projectile-completing-read-file)))

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
(define-key js-test-prefix-map "j" 'jest-test-current-file)
(define-key js-test-prefix-map "n" 'nightwatch-test-current-file)
(define-key js-test-prefix-map "g" 'goto-create-jest-test-for-current-file)

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

;; (use-package typescript-mode
;;   :hook (typescript-mode . setup-js-intellisense)
;;   :mode (("\\.js\\'" . typescript-mode)
;; 	 ("\\.ts\\'" . typescript-mode))
;;   :config
;;   (setq typescript-indent-level 2))

;; (use-package json-mode
;;   :after web-mode
;;   :mode ("\\.json\\'" . json-mode)
;;   :config
;;   (setq json-reformat:indent-width 1))

(autoload 'web-mode "web-mode-hook")

(defun setup-webmode ()
    "Does some setup depending on the current file extension."

    (when-file-extension-matches
     '("[jt]sx" "svelte")
     'setup-js-intellisense))

(add-hook 'web-mode-hook 'setup-webmode)
(add-hook 'web-mode-hook 'maybe-emmet-mode)

;; (("\\.jsx\\'" . web-mode)
;;  ("\\.tsx\\'" . web-mode)
;;  ("\\.vue\\'" . web-mode)
;;  ("\\.[s]?css\\'" . web-mode)
;;  ("\\.cshtml\\'" . web-mode)
;;  ("\\.blade.php\\'" . web-mode)
;;  ("\\.htm[l]?\\'" . web-mode)
;;  ("\\.twig\\'" . web-mode)
;;  ("\\.svelte\\'" . web-mode)
;;  ("\\.ftl\\'" . web-mode))

(with-eval-after-load 'web-mode
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
      ad-do-it)))

(provide 'init-webmode)
;;; init-webmode.el ends here
