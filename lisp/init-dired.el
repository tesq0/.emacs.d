(use-package dired-quick-sort
  :ensure t
  :config
  (dired-quick-sort-setup))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

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

(general-define-key
 :keymaps 'dired-mode-map
 "<" 'nil
 ">" 'nil
 "<normal-state> <mouse-1>" 'dired-find-file
 "<mouse-2>" 'dired-find-file-other-window
 "<mouse-3>" 'dired-up-directory
 "<normal-state> ;" 'evil-forward-char
 "<normal-state> <" 'dired-up-directory
 "<normal-state> >" 'dired-find-file
 "<normal-state> k" 'dired-next-line
 "<normal-state> l" 'dired-previous-line
 "i" 'dired-show-file-type
 "y" 'dired-copy-map
 )

;; allow to change permissions
(setq wdired-allow-to-change-permissions t)

(provide 'init-dired)
