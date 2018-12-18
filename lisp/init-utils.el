(if (fboundp 'with-eval-after-load)
		(defalias 'after-load 'with-eval-after-load)
	(defmacro after-load (feature &rest body)
		"After FEATURE is loaded, evaluate BODY."
		(declare (indent defun))
		`(eval-after-load ,feature
			 '(progn ,@body))))



;;----------------------------------------------------------------------------
;; macros
;;----------------------------------------------------------------------------

(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. DOCSTRING and BODY are as in
`defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "radian-defadvice: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (advice-add ',place ',where #',name)
',name))

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





(defun rename-file-and-buffer (new-name)
	"Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
	(let ((name (buffer-name))
				(filename (buffer-file-name)))
		(if (not filename)
				(message "Buffer '%s' is not visiting a file!" name)
			(if (get-buffer new-name)
					(message "A buffer named '%s' already exists!" new-name)
				(progn	 (rename-file filename new-name 1)	 (rename-buffer new-name)		 (set-visited-file-name new-name)		 (set-buffer-modified-p nil)))))) ;;

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


;; (defun switch-to-window-mru ()
;;   "Move the cursor to the previous (last accessed) buffer in another window.
;; More precisely, it selects the most recently used buffer that is
;; shown in some other window, preferably of the current frame, and
;; is different from the current one."
;; 	(interactive)
;;   (catch 'done
;;     (dolist (buf (buffer-list))
;;       (let ((win (get-buffer-window buf)))
;;         (when (and (not (eq buf (current-buffer)))
;;                    win
;;                    (not (eq win (selected-window))))
;; 					(let ((wframe (window-frame win))
;; 								(sframe (selected-frame)))
;; 					(when (and (frame-live-p wframe)
;; 										 (not (eq wframe sframe)))
;; 						(select-frame-set-input-focus wframe)))
;; 					(select-window win)
;;           (throw 'done nil))))))


;; (defun switch-to-recently-selected-buffer ()
;; 	(interactive)
;; 	(let* ((buflist (buffer-list))      ;   get all buffer list  -- before (selected-frame)
;; 				 (buflist (delq (current-buffer) buflist)))     ; if there are multiple windows showing same buffer.
;; 		(if buflist
;; 				(let (buf (car buflist))
;; 					(switch-to-buffer buf))
;; 			(message "Couldn't find a suitable buffer to switch to"))))

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

(defun explorer ()
	(interactive)
	(when sys/win32p
		(shell-command "explorer .")
		)
	)

(defun switch-to-recently-selected-buffer ()
  "Switch to other buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))


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


;; CoS specific utils

(defun generate-entitas ()
	"Generate Entitas."
	(interactive)
	(let* ((directory (projectile-project-root))
					(default-directory directory)
					(command (concat directory "generate.bat"))
					(buffer (generate-new-buffer "*Generating Entitas*"))
					(display-buffer-alist '(("Generating Entitas" display-buffer-no-window))))
		(with-current-buffer buffer
			(setq buffer-read-only nil)
			(let* ((inhibit-read-only t) proc)
				(erase-buffer))
			(display-buffer buffer '(nil (allow-no-window . t)))
      (setq proc (start-process "Shell" buffer shell-file-name
                                shell-command-switch command))
			(set-process-sentinel proc `(lambda (process signal)

																		(let* ((status (process-status process)))
																		(when (memq status '(exit signal))
																			(cond
																			 ((string= (substring signal 0 -1) "finished")
																				(let* ((cmd (car (cdr (cdr (process-command process))))))
																					(if counsel-etags-debug (message "`%s` executed." cmd))))
																			 (t
																				(message "Failed to generate.")))))))
			;; Use the comint filter for proper handling of carriage motion
			;; (see `comint-inhibit-carriage-motion'),.
			(set-process-filter proc 'comint-output-filter))))

(provide 'init-utils)
;;(display-buffer-pop-up-window buf '((window-height . 40)) )
