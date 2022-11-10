(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(with-eval-after-load 'dired
  (defun my/dired-view ()
    "View files, either as HTML or media"
    (interactive)
    (let* ((files (dired-get-marked-files))
	   (how-many (length files))
	   (extensions (mapcar 'file-name-extension files))
	   (extensions (mapcar 'downcase extensions)))
      (cond ((member "html" extensions) (eww-open-file (car files)))
	    (t (if (> how-many 1) (xdg-open-files files)
		 (xdg-open (car files) t))))))

  (define-key ctl-x-map (kbd "C-d") 'dired)
  (define-key dired-mode-map (kbd "C-c o") 'my/dired-view)
  (define-key dired-mode-map (kbd "C-c C-o") 'my/dired-view)

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
  
  (setq bookmark-alist
	'(( "home" . (filename "~/"))
	  ( "documents" . (filename "~/Documents"))
	  ( "projects" . (filename "~/Projects"))
	  ( "vid" . (filename "~/Videos"))
	  ( "screenshot" . (filename "~/screenshoots"))))


  (define-key dired-copy-map "p" 'dired-copy-file-path)
  (define-key dired-copy-map "n" 'dired-copy-filename-as-kill)

  (defun dired-xdg-open ()
    (interactive)
    (let ((url (dired-get-filename)))
      (try-xdg-open url)))


  (define-key dired-mode-map "i" 'dired-show-file-type)
  (define-key dired-mode-map "y" 'dired-copy-map)
  (define-key dired-mode-map "p" 'dired-paste-map)
  (define-key dired-mode-map (kbd "C-d") 'dired-dragon)
  (define-key dired-mode-map "M" 'dired-mark-unmarked-files)
  (define-key dired-mode-map "M" 'dired-mark-unmarked-files)
  (define-key dired-mode-map (kbd "<mouse-1>") 'dired-mouse-find-file)
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-file-other-window)
  (define-key dired-mode-map (kbd "<mouse-3>") 'dired-up-directory)
  (define-key dired-mode-map "<" 'dired-up-directory)
  (define-key dired-mode-map "g" 'bookmark-jump)
  (define-key dired-mode-map (kbd "C-c o") 'my/dired-view)
  (define-key dired-mode-map (kbd "C-c C-e") 'dired-eval-marked-file)
  (define-key dired-mode-map (kbd "C-x e") 'dired-eval-marked-file)
  (define-key dired-mode-map (kbd "C-x C-e") 'dired-eval-marked-file)
  (define-key dired-mode-map (kbd "C-c C-o") 'my/dired-view)

  (defun dired-dragon (&optional file-list)
    (interactive
     (let ((files (dired-get-marked-files t current-prefix-arg)))
       (list files)))
    (let* ((file-string (string-join file-list " "))
	   (command (format "dragon -a -x %s &" file-string)))
      (shell-command command)))


  ;; allow to change permissions
  (setq wdired-allow-to-change-permissions t
	;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
	dired-auto-revert-buffer t))


(with-eval-after-load 'dired-x
  (defun dired-toggle-show-hidden-files ()
    (interactive)
    (setq dired-show-hidden-files (not dired-show-hidden-files))
    (when (eq major-mode 'dired-mode)
      (let ((prefix-arg (not dired-show-hidden-files)))
	(call-interactively 'dired-omit-mode))))

  (defun dired-maybe-show-hidden-files ()
    (when (not dired-show-hidden-files)
      (dired-omit-mode)))

  (add-hook 'dired-mode-hook 'dired-maybe-show-hidden-files)
  (define-key dired-mode-map "h" 'dired-toggle-show-hidden-files)

    (setq dired-omit-verbose nil
	  dired-kill-when-opening-new-dired-buffer t
	  dired-omit-files
	  (concat dired-omit-files
		  "\\|^.DS_Store\\'"
		  "\\|^\\..+$"
		  "\\|^.project\\(?:ile\\)?\\'"
		  "\\|^.\\(svn\\|git\\)\\'"
		  "\\|^.ccls-cache\\'"
		  "\\|\\(?:\\.js\\)?\\.meta\\'"
		  "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
    
    ;; Disable the prompt about whether I want to kill the Dired buffer for a
    ;; deleted directory. Of course I do!
    (setq dired-clean-confirm-killing-deleted-buffers nil)
    ;; Let OS decide how to open certain files
    (when-let (cmd (cond (sys/macp "open")
			 (sys/linuxp "xdg-open")
			 (sys/win32p "start")))
      (setq dired-guess-shell-alist-user
	    `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
	      ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
	      ("\\.\\(?:xcf\\)\\'" ,cmd)
	      ("\\.csv\\'" ,cmd)
	      ("\\.tex\\'" ,cmd)
	      ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\|webm\\)\\(?:\\.part\\)?\\'" ,cmd)
	      ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
	      ("\\.html?\\'" ,cmd)
	      ("\\.md\\'" ,cmd)))))


(provide 'init-dired)


