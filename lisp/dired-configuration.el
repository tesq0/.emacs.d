(with-eval-after-load 'dired
  (define-key ctl-x-map (kbd "C-d") 'dired)

  (defcustom dired-show-hidden-files nil
    "Whether to show hidden files in dired"
    :group 'dired
    :type 'boolean)

  (defun dired-eval-marked-file ()
    (interactive)
    (let ((path (dired-get-filename)))
      (when path
	(eval-file path))))
  
  (defun dired-copy-file-path ()
    (interactive)
    (let ((path (dired-get-filename)))
      (when path
	(message (format "Copied path %s" path))
	(kill-new path))))

  (define-prefix-command 'dired-copy-map)
  (define-prefix-command 'dired-paste-map)
  (define-prefix-command 'go-map)
  
  (define-key dired-copy-map "p" 'dired-copy-file-path)
  (define-key dired-copy-map "n" 'dired-copy-filename-as-kill)
  (define-key dired-mode-map "i" 'dired-show-file-type)
  (define-key dired-mode-map "y" 'dired-copy-map)
  (define-key dired-mode-map "p" 'dired-paste-map)
  (define-key dired-mode-map "M" 'dired-mark-unmarked-files)
  (define-key dired-mode-map "M" 'dired-mark-unmarked-files)
  (define-key dired-mode-map (kbd "<mouse-1>") 'dired-mouse-find-file)
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-file-other-window)
  (define-key dired-mode-map (kbd "<mouse-3>") 'dired-up-directory)
  (define-key dired-mode-map "<" 'dired-up-directory)
  (define-key dired-mode-map "g" 'bookmark-jump)
  (define-key dired-mode-map (kbd "C-c C-e") 'dired-eval-marked-file)
  (define-key dired-mode-map (kbd "C-x e") 'dired-eval-marked-file)
  (define-key dired-mode-map (kbd "C-x C-e") 'dired-eval-marked-file)

  
  ;; allow to change permissions
  (setq wdired-allow-to-change-permissions t
	;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
	dired-auto-revert-buffer t))




