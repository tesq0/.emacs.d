;;;  Mikus-Csharp-Config --- configuration for csharp


;;; Commentary:

;;; Code:

(require 'open-in-msvs)
(require 'open-in-vscode)

(defun initCsharp ()
	"Initate csharp speciifc stuff."
	(omnisharp-mode)

	(flycheck-mode)

	(setenv "GTAGSLABEL" "pygments")

	(setq-local yas-indent-line 'fixed)
	(yas-minor-mode)
	(yas-reload-all)
	
	(defvar csharp-eldoc-commands (obarray-make 1))
	(set (intern "eldoc-print" csharp-eldoc-commands) 1)

	(setq-local tab-width 4)
	(setq-local c-syntactic-indentation 1)
	(setq-local indent-tabs-mode nil)
	(setq-local eldoc-message-commands csharp-eldoc-commands)
	(setq-local eldoc-idle-delay 0)
	;; (setq omnisharp-debug t)
	(setq-local dabbrev-check-all-buffers nil)
	(setq-local dabbrev-check-other-buffers nil)
	(setq-local company-dabbrev-code-other-buffers nil)
	(setq-local company-backends '(company-files (company-dabbrev-code :with company-gtags company-yasnippet company-keywords) ))
	(setq-local company-manual-completion-fn #'company-omnisharp)
	(local-set-key (kbd "C-c C-c") 'recompile))


(defun maybe-recompile-unity ()
	(when (string-match "IdleGame" (projectile-project-root))
		(save-buffers-and-compile-unity)))

(defun post-setup-csharp ()

	(setq save-project-commands '( maybe-recompile-unity ))

	(evil-add-command-properties 'omnisharp-go-to-definition :jump t)
	(evil-add-command-properties 'omnisharp-go-to-definition-other-window :jump t)

	(c-add-style "mikus-csharp"
							 '("c#"
								 (c-basic-offset . 4)
								 (c-offsets-alist
									(innamespace . +)
									(inline-open . 0)
									(inline-close . 0)
									(inher-cont . c-lineup-multi-inher)
									(arglist-cont-nonempty . 0)
									(arglist-intro . +)
									;; (arglist-close . c-lineup-close-paren)
									(arglist-close . 0)
									(template-args-cont . +))))

	(add-to-list 'c-default-style '(csharp-mode . "mikus-csharp"))
	
	(defun company-omnisharp (command &optional arg &rest ignored)
		"Override the default function."
		(interactive '(interactive))
		"`company-mode' completion back-end using OmniSharp."

		(cl-case command
			(interactive (company-begin-backend 'company-omnisharp))
			(prefix (when (bound-and-true-p omnisharp-mode)
								(omnisharp-company--prefix)))

			(candidates (omnisharp--get-company-candidates arg))

			;; because "" doesn't return everything, and we don't cache if we're handling the filtering
			(no-cache (or (equal arg "")
										(not (eq omnisharp-company-match-type 'company-match-simple))))

			(match (if (eq omnisharp-company-match-type 'company-match-simple)
								 nil
							 0))

			(annotation (omnisharp--company-annotation arg))

			(meta (omnisharp--get-company-candidate-data arg 'DisplayText))

			(require-match 'never)

			(doc-buffer (let ((doc-buffer (company-doc-buffer
																		 (omnisharp--get-company-candidate-data
																			arg 'Description))))
										(with-current-buffer doc-buffer
											(visual-line-mode))
										doc-buffer))

			(ignore-case omnisharp-company-ignore-case)

			(sorted (if (eq omnisharp-company-match-type 'company-match-simple)
									(not omnisharp-company-sort-results)
								t))

			;; Check to see if we need to do any templating
			(post-completion (let* ((json-result (get-text-property 0 'omnisharp-item arg))
															(allow-templating (get-text-property 0 'omnisharp-allow-templating arg)))

												 (omnisharp--tag-text-with-completion-info arg json-result)
												 (when allow-templating
													 ;; Do yasnippet completion
													 (if (and omnisharp-company-template-use-yasnippet (boundp 'yas-minor-mode) yas-minor-mode)
															 (-when-let (method-snippet (omnisharp--completion-result-item-get-method-snippet
																													 json-result))
																 (omnisharp--snippet-templatify arg method-snippet json-result))
														 ;; Fallback on company completion but make sure company-template is loaded.
														 ;; Do it here because company-mode is optional
														 (require 'company-template)
														 (let ((method-base (omnisharp--get-method-base json-result)))
															 (when (and method-base
																					(string-match-p "([^)]" method-base))
																 (company-template-c-like-templatify method-base)))))))))

	)


(use-package omnisharp
	:ensure nil
	:quelpa (omnisharp :fetcher github :repo "tesq0/omnisharp-emacs" :branch "develop")
	:init
	(progn
		(add-hook 'csharp-mode-hook 'initCsharp t)
		(setq omnisharp-imenu-support t)
		(setq omnisharp-eldoc-support t)
		(setq omnisharp-auto-complete-want-documentation t)
		;; (setq omnisharp-debug nil)
		(cond
		 ( sys/macp
			 (setq omnisharp-server-executable-path (concat user-emacs-directory "/.cache/omnisharp/server/v1.32.6/run"))
			 )
		 ( sys/linuxp
			 (setq omnisharp-server-executable-path "omnisharp")
			 )
		 ( sys/win32p
			 (setq omnisharp-server-executable-path (concat user-emacs-directory "\\.cache\\omnisharp\\OmniSharp.exe"))
			 ))

		(general-define-key
		 :keymaps 'csharp-mode-map
		 "M-." 'omnisharp-go-to-definition
		 "M->" 'omnisharp-go-to-definition-other-window
		 "C-c u" 'omnisharp-helm-find-usages
		 "C-c i" 'omnisharp-find-implementations
		 "C-c r" 'omnisharp-run-code-action-refactoring
		 "C-c C-r" 'omnisharp-navigate-to-region
		 "C-c R" 'omnisharp-rename
		 )

		)
	:config
	(post-setup-csharp))

(use-package jetbrains
	:ensure t
	:config
	(progn
		(add-to-list 'jetbrains-major-mode-ide-alist '((csharp-mode . (rider))))
		)
	)

(defvar csharp-org-langs (list "csharp" "cs"))

(defun csharp-check-fontiy (limit)
	"Check if in org source block, then check if the source block is csharp and if so set vars to use csharp highlighting by font-lock."
	(when (org-in-src-block-p)
		(let* ( (lang (car (org-babel-get-src-block-info 'light)))
						(csharp-p (member lang csharp-org-langs))
						(syntax-table (or (and csharp-p csharp-mode-syntax-table) nil))
						(syntax-function (or (and csharp-p #'csharp-mode-syntax-propertize-function) nil)) )

			(message (format "is in csharp block? %s" csharp-p))
			(setq-local font-lock-syntax-table syntax-table)
			(setq-local syntax-propertize-function syntax-function)
			)
		)
	)'


;; Custom non-omnisharp flycheck checker

(defun csharp-set-flycheck-command ()
  "Set the flycheck command, dynamically, as a side effect.
This function is intended for use as a before-syntax-check-hook with
flycheck.  Use it like this:
    (add-hook 'flycheck-before-syntax-check-hook  #'csharp-set-flycheck-command)
Then, in your csharp file, specify this in the comments at the header.
    // flycheck: gmcs -t:module /debug+ -pkg:dotnet %f
This will cause flycheck to run the given command, replacing the %f with
the source file name."

  (and (eq major-mode 'csharp-mode)
       (let ((cmd-string
              (csharp-get-value-from-comments "flycheck" csharp-cmd-line-limit)))
         (and cmd-string
              (not (eq cmd-string ""))
							(let* ((cmd (split-string cmd-string " "))
										 (ferf (member "%f" cmd)))
								(and ferf (setcar ferf 'source))
								(put 'csharp :flycheck-command cmd))))))


(after-load "flycheck"

	(flycheck-define-checker csharp
		"A C# syntax checker for dotnet. By default, it uses the Mono
		 compiler. If you would like to use a different compiler, see
		 `csharp-set-flycheck-command'."
		:command ("mcs" "-target:module" source)
		:error-patterns
		;; WinFormsHello.cs(17,9): error CS0246: The type or namespace name `derp' could not be found. Are you missing an assembly reference?
		((error line-start (file-name) "(" line "," column "): error " (message) line-end)
		 (warning line-start (file-name) "(" line "," column "): warning " (message) line-end))
		:modes csharp-mode)

	;; (add-hook 'flycheck-before-syntax-check-hook  #'csharp-set-flycheck-command)
	)

;; (add-hook 'org-font-lock-hook 'csharp-check-fontiy)
;; (remove-hook 'org-font-lock-hook 'csharp-check-fontiy)

;; (advice-add 'jit-lock-function :before #'csharp-check-fontify)

;; (defun clear-jit-lock-advices ()
;;	(interactive)
;;	(let* ((name "jit-lock-function"))
;;		(advice-mapc #'(lambda (advice props) (general-remove-advice (intern name) advice)) (intern name))
;;		)
;;	)



(provide 'init-csharp)
;;; mikus-csharp.el ends here
