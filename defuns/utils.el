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

;; CODE FOLDING
(defun toggle-selective-display (column)
	(interactive "P")
	(set-selective-display
	 (or column
			 (unless selective-display
				 (1+ (current-column))))))

(defun toggle-hiding (column)
	(interactive "P")
	(if hs-minor-mode
			(if (condition-case nil
							(hs-toggle-hiding)
						(error t))
					(hs-show-all))
		(toggle-selective-display column)))
;; CODE FOLDING END

(provide 'utils)
;;(display-buffer-pop-up-window buf '((window-height . 40)) )
