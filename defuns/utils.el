
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
'(progn ,@body))))


(defun shell-other-window ()
	"Open a `shell' in a new window."
	(interactive)
	(let ((buf (eshell)))
		(switch-to-buffer (other-buffer buf))
		(switch-to-buffer-other-window buf)))


;; YASNIPPET UTILS
(defun find-project-root ()
	(interactive)
	(if (ignore-errors (eproject-root))
			(eproject-root)
		(or (find-git-repo (buffer-file-name)) (file-name-directory (buffer-file-name)))))

(defun find-git-repo (dir)
	(if (string= "/" dir)
			nil
		(if (file-exists-p (expand-file-name "../.git/" dir))
				dir
			(find-git-repo (expand-file-name "../" dir)))))


(defun file-path-to-namespace ()
	(interactive)
	(let (
				(root (find-project-root))
				(base (file-name-nondirectory buffer-file-name))
				)
		(substring (replace-regexp-in-string "/" "\." (substring buffer-file-name (length root) (* -1 (length base))) t t) 0 -1)
		)
	)
;; YASNIPPET UTILS END

(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file filename new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) ;;
;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name nil))))) 



(defun byte-compile-emacs ()
	"A function to byte compile Emacs dir."
	(interactive)
	(byte-recompile-directory (expand-file-name user-emacs-directory) 0))


(defun sudo-edit (&optional arg)
	"Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
	(interactive "P")
	(if (or arg (not buffer-file-name))
			(find-file (concat "/sudo:root@localhost:"
												 (ido-read-file-name "Find file(as root): ")))
		(find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; let's define some ghetoo keybindings

;; (define-prefix-command	'frame-map)
(defun vmake-frame ()
	"Make an Emacs horizontal frame in i3 window manager."
	(interactive)
	(shell-command "i3-msg split v")
	(make-frame))
(defun hmake-frame ()
	"Make an Emacs horizontal frame in i3 window manager."
	(interactive)
	(shell-command "i3-msg split h")
	(make-frame))

(provide 'utils)
;;(display-buffer-pop-up-window buf '((window-height . 40)) )
