(use-package dired-quick-sort
	:ensure t
	:config
	(dired-quick-sort-setup))

(use-package dired-narrow
	:ensure t
	:bind (:map dired-mode-map
							("/" . dired-narrow)))

(after-load 'dired
	(defun dired-copy-file-path ()
		(interactive)
		(let ((path (dired-get-filename)))
			(when path
				(message (format "Copied path %s" path))
				(kill-new path))))

	(define-prefix-command 'dired-copy-map)

	(general-define-key
	 :keymaps 'dired-copy-map
	 "p" 'dired-copy-file-path
	 "n" 'dired-copy-filename-as-kill
	 "y" 'dired-do-copy)

	(defun dired-xdg-open ()
		(interactive)
		(let ((url (dired-get-filename)))
			(try-xdg-open url)))

	(general-define-key
	 :keymaps 'dired-mode-map
	 "<" 'nil
	 ">" 'nil
	 ";" 'nil
	 "i" 'dired-show-file-type
	 "y" 'dired-copy-map
	 "<mouse-1>" 'dired-mouse-find-file
	 "<mouse-2>" 'dired-find-file-other-window
	 "<mouse-3>" 'dired-up-directory
	 :states '(normal motion)
	 ";" 'evil-forward-char
	 "<" 'dired-up-directory
	 ">" 'dired-find-file
	 "k" 'dired-next-line
	 "l" 'dired-previous-line
	 "n" 'evil-search-next
	 "C-c o" 'dired-xdg-open
	 "C-c C-o" 'dired-xdg-open
	 )

	;; allow to change permissions
	(setq wdired-allow-to-change-permissions t)
	(setq dired-auto-revert-buffer t))


(provide 'init-dired)
