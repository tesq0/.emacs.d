;;; init-php --- Summary
;;; Commentary:
;;; Configuration for php-mode

;;; Code:

(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php\\'". php-mode))

(with-eval-after-load 'php-mode

  (with-eval-after-load 'eglot
    (setcdr (assoc '(php-mode phps-mode) eglot-server-programs) '("phpactor" "language-server" "-vvv")))
  
  (defun php-cs-fixer-command-is-ok ()
    "Check if php-cs-fixer is in PATH."
    (if (executable-find "php-cs-fixer")
	t
      (progn (warn "php-cs-fixer not found") nil)))

  (defun find-csfix-config ()
    "Find the nearest php-cs-fixer config file."
    (find-filename-in-project ".php_cs"))

  (defun php-cs-fixer-fix ()
    "Run php cs fixer on the current buffer."

    (when (php-cs-fixer-command-is-ok)

      (let ((tmpfile (make-temp-file "PHP-CS-Fixer" nil ".php"))
	    (patchbuf (get-buffer-create "*PHP-CS-Fixer patch*"))
	    (errbuf (get-buffer-create "*PHP-CS-Fixer stdout*"))
	    (php-cs-fixer-command "php-cs-fixer")
	    (config (find-csfix-config)))

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
			      "--path-mode=intersection"
			      "--quiet"
			      (or (and config
				       (format "--config=%s" config))
				  nil)
			      tmpfile)
		(if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
		    (message "Buffer is already php-cs-fixed")
		  (with-current-buffer (current-buffer)
		    (let ((saved-position (point)))
		      (erase-buffer)
		      (insert-file-contents tmpfile)
		      (goto-char (min saved-position (point-max))))
		    (message "Applied php-cs-fixer"))))
	    (warn (with-current-buffer errbuf (buffer-string))))))))

  (defun php-cs-fixer-before-save ()
    "Used to automatically fix the file saving the buffer.
Add this to .emacs to run php-cs-fix on the current buffer when saving:
 (add-hook 'before-save-hook 'php-cs-fixer-before-save)."

    (interactive)
    (when (and
	   buffer-file-name
	   (string= (file-name-extension buffer-file-name) "php")
	   (or (not (boundp geben-temporary-file-directory))
	       (not (string-match geben-temporary-file-directory (file-name-directory buffer-file-name))))
	   ) (php-cs-fixer-fix)))

  (defun insert-psr4-namespace ()
    "Try auto resolving and inserting the current psr-4 namespace."
    (interactive)
    (let ((composer-json-path (find-filename-in-project "composer.json")))
      (if composer-json-path
	  (let* ((composer-json
		  (json-read-file composer-json-path))
		 (psr-4
		  (alist-get 'psr-4
			     (alist-get 'autoload composer-json))))
	    (dolist (nsr psr-4)
	      (let* ((root (file-name-directory composer-json-path))
		     (key (car nsr))
		     (val (string-trim (cdr nsr) "/" "/"))
		     (r-path
		      (replace-regexp-in-string
		       (regexp-quote root)
		       ""
		       (file-name-directory (buffer-file-name))))
		     (rp-list
		      (split-string (string-trim r-path "/" "/") "/"))
		     (r-value
		      (car rp-list)))

		(when (string-equal val r-value)
		  (insert
		   (format "namespace %s%s;"
			   key
			   (string-join
			    (cdr rp-list)
			    "\\")
			   ))
		  (cl-return nil))
		)
	      ))
	(warn "Could not find composer.json in project")
	)
      ))

  (defun setup-php ()
    "Configure local stuff when changing to php-mode."
    (setq-local c-basic-offset 4)

    (when (eq (buffer-size (current-buffer)) 0)
      (insert "<?php")
      (newline 2)
      (insert-psr4-namespace)
      (call-interactively 'end-of-buffer)
      (newline 2)))

  (add-hook 'php-mode-hook 'setup-php))
