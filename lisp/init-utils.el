(defgroup utils nil
  "Bunch of utility functions"
  :group 'init)


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
	(pos 0)
	(group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]*$" "" str))

;; Find the directory containing a given library
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let* ((pattern (concat "^" (file-name-as-directory directory))))
    (if (string-match-p pattern file) file)))


(defmacro my-select-from-kill-ring (fn &optional n)
  "Use `browse-kill-ring' if it exists and N is 1.
If N > 1, assume just yank the Nth item in `kill-ring'.
If N is nil, use `ivy-mode' to browse the `kill-ring'."
  (interactive "P")
  `(cond
    ((or (not ,n) (and (= ,n 1) (not (fboundp 'browse-kill-ring))))
     ;; remove duplicates in `kill-ring'
     (let* ((candidates (cl-remove-if
			 (lambda (s)
			   (or (< (length s) 5)
			       (string-match "\\`[\n[:blank:]]+\\'" s)))
			 (delete-dups kill-ring))))
       (let* ((ivy-height (/ (frame-height) 2)))
	 (ivy-read "Browse `kill-ring':"
		   (mapcar
		    (lambda (s)
		      (let* ((w (frame-width))
			     ;; display kill ring item in one line
			     (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
			;; strip the whitespace
			(setq key (replace-regexp-in-string "^[ \t]+" "" key))
			;; fit to the minibuffer width
			(if (> (length key) w)
			    (setq key (concat (substring key 0 (- w 4)) "...")))
			(cons key s)))
		    candidates)
		   :action #',fn))))
    ((= ,n 1)
     (browse-kill-ring))))

(defun my-insert-str (str)
  ;; ivy8 or ivy9
  (if (consp str) (setq str (cdr str)))
  ;; evil-mode?
  (if (and (functionp 'evil-normal-state-p)
	   (boundp 'evil-move-cursor-back)
	   (evil-normal-state-p)
	   (not (eolp))
	   (not (eobp)))
      (forward-char))
  ;; insert now
  (insert str))

(defun my-line-str (&optional line-end)
  (buffer-substring-no-properties (line-beginning-position)
				  (if line-end line-end (line-end-position))))

(defun my-buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun my-selected-str ()
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun my-use-selected-string-or-ask (hint)
  "Use selected region or ask user input for string."
  (if (region-active-p) (my-selected-str)
    (if (string= "" hint) (thing-at-point 'symbol)
      (read-string hint))))

;;Some other manipulation utils

;; (defun next-line-and-indent
;;		)


(defun kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (end-of-line);;(forward-char 1)
	     (just-one-space 0)
	     (backward-char 1)
	     (kill-line arg))
    (kill-line arg)))


(defun power-shell ()
  (interactive)
  (shell-command "start powershell"))

(defun terminal-sentinel (process event)
  "Kill buffer and window on shell PROCESS, EVENT termination."
  (when (not (process-live-p process))
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
	(with-current-buffer buf
	  (kill-buffer))))))

(defun my-term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))

(advice-add 'term-handle-exit :after 'my-term-handle-exit)

(defun terminal ()
  (interactive)
  (ansi-term (getenv "SHELL")))

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

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

;; Delete the current file
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
			     (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; (defun stage-this-file ()
;;	(interactive)
;;	(or (buffer-file-name) (error "No file is currently being edited"))
;;	(when (yes-or-no-p (format "Stage file '%s'?"
;;														 (file-name-nondirectory buffer-file-name)))

;;		(shell-command (format "cd $(cygpath -w %s); git add $(cygpath -w %s)" (pwd) (buffer-file-name)))))



(defun copy-current-file-name ()
  (interactive)
  (kill-new (buffer-file-name)))


(defun rename-file-and-buffer (&optional rename)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (let*  ((name (buffer-name))
	  (filename (buffer-file-name))
	  (new-name (or (and rename (funcall rename filename)) (read-from-minibuffer "New name: " name))))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn	 (rename-file filename new-name 1)	 (rename-buffer new-name)		 (set-visited-file-name new-name)		 (set-buffer-modified-p nil))))
    ))

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
      (progn	(copy-file filename newname 1)	(delete-file filename)	(set-visited-file-name nil)))))

(defun unique-buffer-p (buffer)
  "Check whether a BUFFER has an unique name (if it's surrounded with **, e.g *scratch*)."
  (s-matches? "\*.*\*$" (buffer-name buffer)))

(defun kill-abandoned-buffers ()
  "Deletes buffers which files have been removed but they still exists in the buffer list."
  (interactive)
  (dolist (buffer (buffer-list))
    (let* ((buffer-file (buffer-file-name buffer))
	   (file-exists? (and buffer-file (file-exists-p buffer-file)))
	   (unique-p (unique-buffer-p buffer)))
      (and (not file-exists?) (not unique-p)
	   (progn
	     (message (format "Killing abandoned buffer %s" (buffer-name buffer)))
	     (kill-buffer buffer))))))

(defun switch-to-the-window-that-displays-the-most-recently-selected-buffer ()
  (interactive)
  (let* ((buflist (buffer-list))      ;   get all buffer list  -- before (selected-frame)
	 (buflist (delq (current-buffer) buflist))     ; if there are multiple windows showing same buffer.
	 (winlist (mapcar (lambda (x) (get-buffer-window x t)) buflist)) ; buf->win
	 (winlist (delq nil winlist))                  ; remove non displayed windows
	 (winlist (delq (selected-window) winlist)))   ; remove current-window
    (if winlist

	(let* ((win (car winlist))
	       (wframe (window-frame win))
	       (sframe (selected-frame)))
	  (when (and (frame-live-p wframe)
		     (not (eq wframe sframe)))
	    (select-frame-set-input-focus wframe))
	  (if (window-live-p win)
	      (select-window win)
	    (error "Dead window %S" win)))
      (message "Couldn't find a suitable window to switch to"))))

(defun reopen-buffer ()
  "Kill and open current BUFFER."
  (interactive)
  (let ( (buffer (buffer-name))
	 (file (buffer-file-name))
	 (point (point)) )
    (kill-buffer buffer)
    (find-file file)
    (goto-char point)))


(defun dos2unix (buffer)
  "Convert BUFFER from DOS file format to UNIX."
  (interactive "*b")
  (shell-command (format "dos2unix %s" (file-truename buffer))))

(defun ranger ()
  (interactive) (terminal "-e ranger"))

(defun explorer ()
  (interactive)
  (cond ( sys/win32p
	  (shell-command "explorer ."))
	( sys/linuxp
	  (try-xdg-open default-directory))))

(defun try-xdg-open (URL)
  (if (and (stringp URL) (not (string-empty-p URL)) (executable-find "xdg-open"))
      (start-process "xdg-open" nil
		     "xdg-open" URL)
    nil))

(defun xdg-open (file &optional async)
  "Opens file with xdg-open. Without optional argument ASYNC, it will wait for the file to finish playing or review."
  (let ((command (format "xdg-open '%s'" file))
	(process-connection-type nil))
    (if async
	(async-shell-command command)
      (shell-command command))))

(defun xdg-open-files (files)
  "Opens list of files with xdg-open one by one, waiting for each to finish."
  (dolist (file files)
    (xdg-open file)))

(defun switch-to-recently-selected-buffer ()
  "Switch to other buffer"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer ) 1)))


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
  (let ((saved-point (point)))
    (if (or arg (not buffer-file-name))
	(find-file (concat "/sudo:root@localhost:"
			   (ido-read-file-name "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))
    (goto-char saved-point)))

(defun compile-unity ()
  (interactive)
  (let* ((shell-output (shell-command-to-string "xwininfo -tree -root | grep 'Unity.*Personal' | awk '{ print $1; }'"))
	 (window-id (replace-regexp-in-string "\n" "\s" shell-output))
	 (command (format "xdotool windowactivate --sync %s key 'Control_R+r'" window-id)))
    (message "command %s" command)
    (shell-command command)))

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t))

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

(defun string-append-to-uppercase-chars (string string-to-append)
  "Append STRING-TO-APPEND to uppercase characters in STRING."
  (replace-regexp-in-string "\\([A-Z]\\)" (format "\\1%s" string-to-append) string))

(defun string-camel-to-burger-case (string)
  "Change camelCase STRING to burger-case."
  (downcase (string-append-to-uppercase-chars string "-")))

(defun string-camel-to-caps (string)
  "Change camelCase STRING to CAPS_CASE."
  (upcase (string-append-to-uppercase-chars string "_")))

;; (string-camel-to-caps "camelCase")

;; (string-camel-to-burger-case "camelCase")

;; (replace-regexp-in-string "\\([A-Z]\\)" "-\\1" "CamelCale")

(defun modify-word-at-point (fn)
  "Modify word at point with FN."
  (interactive)
  (let* ((case-fold-search nil)
	 (bounds (bounds-of-thing-at-point 'word))
	 (s (car bounds))
	 (e (cdr bounds))
	 (word (buffer-substring-no-properties s e))
	 (new-word (funcall fn word)))
    (delete-region s e)
    (goto-char s)
    (insert (format "\"%s\"" new-word))))

(defun camel-to-burger-case ()
  "Convert word at point from camelCase to burger-case."
  (interactive)
  (modify-word-at-point
   (lambda (word)
     (string-camel-to-burger-case word))))

(defun evil-find-WORD (forward)
  "Return WORD near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (evil-find-thing forward 'evil-WORD))

(defun copy-word-from-above ()
  "Copies the first found word from the line above."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line -1)
      (evil-goto-column col)
      (kill-new (evil-find-WORD t))))
  (yank))

(defun format-find-find-command (dir search &optional regexp-p)
  (format "find %s -type f %s"
	  dir
	  (format "%s '%s'" (or (and regexp-p "-regex") "-name") search)))

(defun format-fd-find-command (dir search &rest rest)
  (format "fd --hidden -t f %s %s" search dir))

(defvar format-find-command 'format-fd-find-command)

(defvar find-sort-command "awk '{ print length, $0 }' | sort -n -s | cut -d \" \" -f2-")

(defun find-files (dir search &optional regexp-p)
  "Find files in DIR matching SEARCH.
If REGEXP-P is non-nil, treat SEARCH as a regex expression."
  (split-string
   (string-trim
    (shell-command-to-string
     (format "%s | %s" (apply format-find-command dir search regexp-p) find-sort-command))
    "\n" "\n") "\n"))

(defun find-filename-in-project (filename)
  "Find the nearest FILENAME starting from project root."
  (let* ((dir (or
	       (and (fboundp 'projectile-project-root)
		    (projectile-project-root))
	       default-directory))
	 (file-at-root-lvl (format "%s/%s" dir filename)))
    (or (and (file-exists-p file-at-root-lvl) file-at-root-lvl)
	(car (find-files dir filename))
	)))

(defun file-class-name ()
  (interactive)
  (save-excursion
    (call-interactively 'move-end-of-line)
    (search-backward-regexp "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?class\\s-+\\(\\(?:\\sw\\|\\\\\\|\\s_\\)+\\)")
    (let ((ret (match-string 1)))
      (message ret)
      ret)))

(defun ensure-list (obj)
  "Wraps OBJ in a list if it's not already."
  (or (and (listp obj) obj)
      (list obj)))


(defun mapcheck-while (list check-fn fn)
  "Call CHECK-FN on every element of LIST, if non-nil, call FN, and end the loop."
  (and (listp list)
       (let ((el (car list)))
	 (or (eq el nil)
	     (when (funcall check-fn el)
	       (funcall fn) t)
	     (mapcheck-while (cdr list) check-fn fn)))))

(defun when-file-extension-matches (exts fn)
  "Do FN when any of EXTS matches current buffer's file ext."
  (let* ((file-extension (file-name-extension buffer-file-name)))
    (mapcheck-while (ensure-list exts) (lambda (ext) (string-match ext file-extension)) fn)))


(defun file-basename (filename)
  (replace-regexp-in-string "\\..*" "" (file-name-nondirectory filename)))

(defun file-path-no-extension (filepath)
  (concat (file-name-directory filepath) (file-basename filepath)))

(defun my/file-relative-name (filename &rest rest)
  (let ((r (apply 'file-relative-name filename rest)))
    (if (char-equal (car (string-to-list r)) (string-to-char "."))
	r
      (format "./%s" r))))

(defun eval-file (path)
  (let ((default-directory (file-name-directory path)))
    (with-temp-buffer
      (insert-file-contents path)
      (eval-buffer))))

(defun async-shell-command-in-buffer-or-switch-to-buffer (command buffer-name)
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
	(switch-to-buffer buffer)
      (async-shell-command
       command (get-buffer-create buffer-name))
      )))

(defun async-shell-command-confirm-kill-process (&rest args)
  (let ((async-shell-command-buffer 'confirm-kill-process))
    (apply #'async-shell-command args)))

(defun rename-file-and-buffer-to-word-at-point ()
  (interactive)
  (rename-file-and-buffer (lambda (filename)
			    (format "%s.%s"
				    (word-at-point t)
				    (file-name-extension filename)))))

(defun insert-buffer-basename ()
  (interactive)
  (insert (file-basename (buffer-file-name))))

(provide 'init-utils)
;;(display-buffer-pop-up-window buf '((window-height . 40)) )
