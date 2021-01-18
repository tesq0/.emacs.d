(use-package helm-projectile
  :ensure t
  :diminish
  :init
  (progn
    (projectile-mode)
    (setq projectile-enable-caching t
	  helm-projectile-fuzzy-match nil)
    (helm-projectile-on)
    (define-key projectile-command-map (kbd "<ESC>") nil)
    (defvar my-find-command)
    (setq my-find-command (or (and sys/win32p "mfd.rb") "fd --hidden --exclude '.git' -t f . -0"))
    (setq-default projectile-git-command my-find-command)
    (setq-default projectile-generic-command my-find-command)
    (setq projectile-indexing-method 'alien)
    (setq projectile-tags-backend 'auto)
    (define-prefix-command 'mikus-tags-map)
    (define-prefix-command 'mikus-search-map)
    (general-define-key
     :keymaps 'projectile-command-map
     "ESC" 'keyboard-quit
     "<tab>" 'projectile-project-buffers-other-buffer)

    (defvar save-project-commands '(save-all-buffers))

    (defun save--project ()
      (interactive)
      (if (listp save-project-commands)
	  (dolist (fn save-project-commands)
	    (funcall fn))))

    (mikus-leader "s" 'mikus-search-map)

    (general-define-key
     :keymaps 'projectile-command-map
     "R" 'projectile-regenerate-tags-async
     "r" 'mikus-tags-map
     "s" 'save--project)
    (general-define-key
     :keymaps 'mikus-search-map
     "a" 'projectile-ag)
    )
  (setq projectile-tags-backend '(etags-select))
  )

(use-package imenu-anywhere
  :ensure t
  :init
  (progn
    (mikus-leader "I" 'imenu-anywhere)))

;; Tags
(after-load 'helm-projectile

  (defvar projectile-custom-ignored-files '())

  (defun tags-custom-ignored-files ()
    projectile-custom-ignored-files)

  (defun projectile-tags-exclude-patterns ()
    "Return a string with exclude patterns for ctags."
    (mapconcat (lambda (pattern) (format "--exclude=\"%s\""
					 (directory-file-name pattern)))
	       (append (tags-custom-ignored-files) (projectile-ignored-directories-rel)) " "))

  (defvar projectile-tags-files-to-process "")
  (defvar projectile-tags-file-extensions "")

  (defun projectile-regenerate-gtags (&optional dirs)
    "First output the files in DIRS we want to parse to gtags.files, then run gtags in project-root."
    (interactive)
    (let* ((default-directory (projectile-project-root))
	   (file-extension (or projectile-tags-file-extensions "."))
	   (directories (or dirs projectile-tags-files-to-process default-directory))
	   (find-command (format "fd %s %s > %sgtags.files" file-extension directories default-directory))
	   (gtags-command (format "gtags -v %s" (directory-file-name default-directory))))
      (message (format "find files command %s" find-command))
      (shell-command find-command)
      (async-shell-command gtags-command)
      ))

  (defun projectile-regenerate-tags-async (&optional files append)
    "Regenerate the project's [e|g]tags.  Optionally specify FILES."
    (interactive)
    (let* ((project-root (projectile-project-root))
	   (tags-exclude (projectile-tags-exclude-patterns))
	   (default-directory project-root)
	   (tags-file (expand-file-name projectile-tags-file-name))
	   (command (format projectile-tags-command tags-file tags-exclude (or files default-directory))))
      (message "regenerate tags command: %s" command)
      (async-shell-command command)))


  (general-define-key
   :keymaps 'mikus-tags-map
   "r" 'projectile-regenerate-tags
   "R" 'projectile-regenerate-tags-async)


  )
(provide 'init-projectile)
