
(after-load 'dired
  
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
  (define-key dired-mode-map (kbd "o") 'my/dired-view)

  (defcustom dired-show-hidden-files nil
    "Whether to show hidden files in dired"
    :group 'dired
    :type 'boolean)
  
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
  
  (general-define-key
   :keymaps 'dired-copy-map
   "p" 'dired-copy-file-path
   "n" 'dired-copy-filename-as-kill)

  (defun dired-xdg-open ()
    (interactive)
    (let ((url (dired-get-filename)))
      (try-xdg-open url)))

  (general-unbind dired-mode-map
    "<" ">" ";" "e" "v" "g" "N")

  (defun dired-dragon (&optional file-list)
    (interactive
     (let ((files (dired-get-marked-files t current-prefix-arg)))
       (list files)))
    (let* ((file-string (string-join file-list " "))
	   (command (format "dragon -a -x %s &" file-string)))
      (shell-command command)))
  
  (general-define-key
   :keymaps 'dired-mode-map
   "i" 'dired-show-file-type
   "y" 'dired-copy-map
   "p" 'dired-paste-map
   "C-d" 'dired-dragon
   "M" 'dired-mark-unmarked-files
   "M" 'dired-mark-unmarked-files
   "<mouse-1>" 'dired-mouse-find-file
   "<mouse-2>" 'dired-find-file-other-window
   "<mouse-3>" 'dired-up-directory
   :states '(normal motion)
   ";" 'evil-forward-char
   "e" 'evil-forward-word-end
   "<" 'dired-up-directory
   "g" 'bookmark-jump
   ">" 'dired-find-file
   "k" 'dired-next-line
   "l" 'dired-previous-line
   "n" 'evil-search-next
   "C-c o" 'dired-view-file
   "C-c C-o" 'dired-view-file
   )

  ;; ranger-like controls
  (use-package dired-ranger
    :ensure t
    :init
    (general-define-key
     :keymaps 'dired-copy-map
     "y" 'dired-ranger-copy)
    (general-define-key
     :keymaps 'dired-paste-map
     "p" 'dired-ranger-paste
     "m" 'dired-ranger-move))

  ;; colourful
  (use-package diredfl
    :ensure nil
    :quelpa (diredfl :fetcher github :repo "purcell/diredfl")
    :hook (dired-mode . diredfl-mode))

  (use-package dired-x
    :ensure nil
    :demand t
    :init
    
    (defun dired-toggle-show-hidden-files ()
      (interactive)
      (setq dired-show-hidden-files (not dired-show-hidden-files))
      (when (eq major-mode 'dired-mode)
	(let ((prefix-arg (not dired-show-hidden-files)))
	  (call-interactively 'dired-omit-mode))))

    (defun dired-maybe-show-hidden-files ()
      (when (not dired-show-hidden-files)
	(dired-omit-mode)
	)
      )

    (add-hook 'dired-mode-hook 'dired-maybe-show-hidden-files)
    
    (general-define-key
     :keymaps 'dired-mode-map
     :states '(normal motion)
     "h" 'dired-toggle-show-hidden-files)

    :config
    (setq dired-omit-verbose nil
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
	      ("\\.md\\'" ,cmd))))
    )

  ;; allow to change permissions
  (setq wdired-allow-to-change-permissions t
	;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
	dired-auto-revert-buffer t)
  )


(provide 'init-dired)
