;;; init-php --- Summary
;;; Commentary:
;;; Configuration for php-mode

;;; Code:

(defun setup-php ()
	"Configure local stuff when changing to php-mode."
	(setenv "GTAGSLABEL" "pygments")
	(setq-local c-basic-offset 4)
	(lsp)
	(yas-minor-mode)
	(setq-local company-backends '(company-files (company-dabbrev-code :with company-capf company-yasnippet company-keywords) ))
	(setq-local company-manual-completion-fn #'company-capf))

(defun php-cs-fixer-command-is-ok ()
	"Check if php-cs-fixer is in PATH."
	(if (executable-find "php-cs-fixer")
			t
		(progn (warn "php-cs-fixer not found") nil)))

(defun php-cs-fixer-fix ()
	"Run php cs fixer on the current buffer."

	(when (php-cs-fixer-command-is-ok)

		(let ((tmpfile (make-temp-file "PHP-CS-Fixer" nil ".php"))
					(patchbuf (get-buffer-create "*PHP-CS-Fixer patch*"))
					(errbuf (get-buffer-create "*PHP-CS-Fixer stdout*"))
					(php-cs-fixer-command "php-cs-fixer"))

			(save-restriction
				(widen)
				(if errbuf
						(with-current-buffer errbuf
							(setq buffer-read-only nil)
							(erase-buffer)))
				(with-current-buffer patchbuf
					(erase-buffer))

				(write-region nil nil tmpfile)

			(if (zerop (call-process "php" nil errbuf nil "-l" tmpfile))
					(progn
						(call-process php-cs-fixer-command
													nil errbuf nil
													"fix"
													"--using-cache=no"
													"--quiet"
													tmpfile)
						(if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
								(message "Buffer is already php-cs-fixed")
							(with-current-buffer (current-buffer)
								(erase-buffer)
								(insert-file-contents tmpfile)
								(message "Applied php-cs-fixer")))
				(warn (with-current-buffer errbuf (buffer-string)))))))))

(defun php-cs-fixer-before-save ()
	"Used to automatically fix the file saving the buffer.
Add this to .emacs to run php-cs-fix on the current buffer when saving:
 (add-hook 'before-save-hook 'php-cs-fixer-before-save)."

	(interactive)
	(when (and
				 buffer-file-name
				 (string= (file-name-extension buffer-file-name) "php")
				 (or (not (boundp 'geben-temporary-file-directory))
						 (not (string-match geben-temporary-file-directory (file-name-directory buffer-file-name))))
				 ) (php-cs-fixer-fix)))


(defun toggle-php-formating ()
	"Toggles php-cs-fixer after save when changing major mode."
	(if (memq major-mode '(php-mode))
			(add-hook 'before-save-hook #'php-cs-fixer-before-save)
		(remove-hook 'before-save-hook #'php-cs-fixer-before-save)))

(use-package php-mode
	:ensure t
	:init
	(add-hook 'php-mode-hook 'setup-php)
	(add-hook 'after-change-major-mode-hook #'toggle-php-formating))

(use-package geben
	:ensure t)

(provide 'init-php)
