(autoload 'projectile "projectile-mode")
(add-hook 'prog-mode-hook 'projectile-mode)

(with-eval-after-load 'projectile

  (define-prefix-command 'mikus-tags-map)
  
  (setq projectile-enable-caching t)

  (define-key projectile-command-map (kbd "<ESC>") nil)

  (defvar my-find-command)
  (setq my-find-command "fd --no-ignore-vcs --ignore-file .gitignore --hidden --exclude '.git' -t f . -0")
  (setq-default projectile-git-command my-find-command)
  (setq-default projectile-generic-command my-find-command)
  (setq projectile-indexing-method 'alien)
  (setq projectile-tags-backend 'auto)

  (define-key projectile-command-map "ESC" 'keyboard-quit)
  (define-key projectile-command-map "<tab>" 'projectile-project-buffers-other-buffer)

  (defvar save-project-commands '(save-all-buffers))

  (defun save--project ()
    (interactive)
    (if (listp save-project-commands)
	(dolist (fn save-project-commands)
	  (funcall fn))))

  (defun projectile-completing-read-file (&rest rest)
    (-if-let* ((project-root (projectile-acquire-root))
	       (file (apply 'projectile-completing-read "Find file:" (projectile-project-files project-root) rest))
	       (path (expand-file-name file project-root)))
	      path
	      (error "Failed to read project file")))

  (defun projectile-eval-file (&optional filename)
    (interactive)
    (let ((path (projectile-completing-read-file :initial-input (or filename ""))))
      (if (string-equal (file-name-extension path) "el")
	  (eval-file path)
	(warn "Selected non-elisp file."))))

  (defun projectile-eval-start-or-file ()
    (interactive)
    (projectile-eval-file "start.el"))

  (defun projectile-terminal ()
    "Open a terminal in project root."
    (interactive)
    (let
	((default-directory (projectile-ensure-project (projectile-project-root))))
      (terminal)))

  (define-key projectile-command-map "R" 'projectile-regenerate-tags-async)
  (define-key projectile-command-map "t" 'projectile-terminal)
  (define-key projectile-command-map "r" 'mikus-tags-map)
  (define-key projectile-command-map "s" 'save--project)
  (define-key projectile-command-map "e" 'projectile-eval-start-or-file)
  (define-key projectile-command-map "E" 'projectile-eval-file)

  (define-key mikus-search-map "a" 'projectile-ag)

  (setq projectile-tags-backend '(etags-select))

  ;; Tags

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
	   (find-command (format "fd -t f %s %s > %sgtags.files" file-extension directories default-directory))
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


  (define-key mikus-tags-map "r" 'projectile-regenerate-tags)
  (define-key mikus-tags-map "R" 'projectile-regenerate-tags-async))

(provide 'init-projectile)
