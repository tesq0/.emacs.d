(defun mikus-evil-setup-command-props ()
	(let ((functions '(evil-find-char
										 evil-find-char-backward
										 evil-find-char-to
										 evil-find-char-to-backward
										 evil-first-non-blank
										 evil-last-non-blank)))
		(dolist (fn functions)
			(evil-add-command-properties fn :jump t))))


(use-package evil
	:ensure t
	:init
	(progn
		(setq evil-mode-line-format nil
					evil-insert-state-cursor '(bar "White")
					evil-normal-state-cursor '(box "White")
					evil-visual-state-cursor '(box "#F86155"))
		(setq evil-move-cursor-back nil)
		(setq evil-search-module 'evil-search)
		(setq evil-want-C-d-scroll nil
					evil-want-C-u-scroll nil)
		(setq evil-ex-search-persistent-highlight nil)
		(setq evil-want-fine-undo nil)
		;; (custom-reevaluate-setting 'evil-overriding-maps)
		)
	:config
	(mikus-evil-setup-command-props)
	(evil-set-initial-state 'term-mode 'emacs)
	(evil-set-initial-state 'calendar-mode 'emacs)
	(evil-set-initial-state 'magit-mode 'emacs)
	(evil-set-initial-state 'eww-mode 'emacs)
	(evil-set-initial-state 'image-mode 'emacs)
	(evil-set-initial-state 'rg-mode 'normal)
	(evil-set-initial-state 'helm-occur 'normal)
	(evil-set-initial-state 'package-menu-mode 'emacs)
	(cl-pushnew (cons 'wgrep-mode-map nil) evil-overriding-maps)
	(setq initial-major-mode 'evil-mode)                 ; set the mode of the initial scratch buffer
	;; :init
	;; (add-hook 'indium-repl-mode-hook 'evil-mode nil)
	)

;;; SUPPLEMENTARY PACKAGES

(use-package avy
	:ensure t
	:init
	;; This is the default
	;; (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
	;; Any lower-case letter or number.  Numbers are specified in the keyboard
	;; number-row order, so that the candidate following '9' will be '0'.
	(progn
		(setq avy-keys (number-sequence ?a ?z))
		(setq avy-all-windows 'all-frames))
	:config
	(defun avy-goto-paren-left ()
		(interactive)
		(avy--generic-jump
		 "("
		 nil
		 'at
		 ))
	(defun avy-goto-paren-right ()
		(interactive)
		(avy--generic-jump
		 ")"
		 nil
		 'at
		 ))

	(defun avy-goto-word-1-in-line (char &optional arg)
		(interactive (list (read-char "char:" t)
											 current-prefix-arg))
		(avy-goto-word-1
		 char
		 arg
		 (line-beginning-position)
		 (line-end-position)))
	)

(use-package evil-surround
	:ensure t
	:init
	(global-evil-surround-mode))


(use-package evil-nerd-commenter
	:ensure t)

(use-package drag-stuff
	:ensure t
	)


;;; Start evil

(evil-mode 1)

;;; DEFUNS

;;; kbd quit
(defun minibuffer-keyboard-quit ()
	"Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
	(interactive)
	(if (and delete-selection-mode transient-mark-mode mark-active)
			(setq deactivate-mark  t)
		(when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
		(abort-recursive-edit)))

(defun my-move-key (keymap-from keymap-to key)
	"Moves key binding from one keymap to another, deleting from the old location. "
	(define-key keymap-to key (lookup-key keymap-from key))
	(define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map " ")


(defun my/evil-switch-emacs-state ()
	"Switch between Emacs and evil state."
	(interactive)
	(if (evil-emacs-state-p)
			(evil-exit-emacs-state)
		(evil-emacs-state)))


(defun insert-line-below (times)
  "Insert an empty line below the current line."
  (interactive "p")
  (save-excursion
    (end-of-line)
    (open-line times)))

(defun insert-line-above (times)
  "Insert an empty line above the current line."
  (interactive "p")
  (save-excursion
    (end-of-line 0)
    (open-line times)))

(defun evil-keyboard-quit ()
	"Keyboard quit and force normal state."
	(interactive)
	(and evil-mode (evil-force-normal-state))
	(if (not auto-hscroll-mode)
			(setq auto-hscroll-mode t))
	(keyboard-quit))

(defun go-start-of-line ()
	"go to the start of line"
	(interactive)
	(if (not auto-hscroll-mode)
			(setq auto-hscroll-mode t))
	(evil-beginning-of-line))

(defun go-end-of-line ()
	"go to the end of line"
	(interactive)
	(if (not auto-hscroll-mode)
			(setq auto-hscroll-mode t))
	(evil-end-of-line))

(defun go-end-of-visual-line ()
	"go to the end of visual line"
	(interactive)
	(if (not auto-hscroll-mode)
			(setq auto-hscroll-mode t))
	(evil-end-of-visual-line))

(defun go-start-of-visual-line ()
	"go to the start of visual line"
	(interactive)
	(if (not auto-hscroll-mode)
			(setq auto-hscroll-mode t))
	(evil-beginning-of-visual-line))

(defun my/evil-get-substitute-pattern (useExPattern)
	(or (and evil-ex-search-pattern
					 (or useExPattern (evil-visual-state-p) )
					 (replace-regexp-in-string
						"[\\<>]" "" (car evil-ex-search-pattern )))
			(word-at-point)))

(defun my/evil-get-substitute-beg-end ()
	(if (evil-visual-state-p)
			"'<,'>" ""))


(defun my/make-group-pattern (pattern)
	(format "\\(%s\\)" pattern))

(defun my/evil-substitute (global &optional useExPattern)
	"Start evil ex with some predefinded text for substitution.
	 GLOBAL - says whether to use the global %s prefix
	 USEEXPATTERN - whether to try to use the pattern form evil-ex-search-pattern."
	(let* ((pattern (my/evil-get-substitute-pattern (and global useExPattern)))
				(command (format "%s%s/%s/"
												 (my/evil-get-substitute-beg-end)
												 (if global "%s" "s")
												 (my/make-group-pattern pattern)
												 )))
		(if pattern
				(evil-ex command)
			(message "pattern is nil"))))

(defun my/evil-line-substitute ()
	"Substitute in a line."
		(interactive)
		(my/evil-substitute nil))

(defun my/evil-global-substitute (x)
	"Substitute globally."
		(interactive "p")
		(my/evil-substitute t (not (eq x 1))))


(defun evil-join-and-indent-upwards ()
	(interactive)
	(join-line)
	(c-indent-command))


(evil-define-operator evil-join-and-indent (beg end)
	"Join the selected lines."
	:motion evil-line
	(let ((count (count-lines beg end)))
		(when (> count 1)
			(setq count (1- count)))
		(goto-char beg)
		(dotimes (var count)
			(progn
				(join-line 1)
				(c-indent-command))
			)))

(defun evil-enter-insert-and-intent ()
	(interactive)
	(c-indent-command)
	(when (not (evil-insert-state-p))
		(evil-insert-state))
	)

(evil-define-avy-motion avy-goto-paren-left inclusive)
(evil-define-avy-motion avy-goto-paren-right inclusive)
(evil-define-avy-motion avy-goto-word-1-in-line inclusive)


	;;; Motion
(evil-define-motion move-2-lines-down ()
	(evil-next-visual-line 2))
(evil-define-motion move-2-lines-up ()
	(evil-previous-visual-line 2))



(with-eval-after-load 'evil-maps

	(global-set-key (kbd "<f9>") 'repeat-complex-command)
	(global-set-key (kbd "C-S-v") 'evil-paste-before)
	(global-set-key (kbd "C-S-c") 'evil-yank)

	(general-define-key
	 :keymaps '(minibuffer-local-map
							minibuffer-local-ns-map
							minibuffer-local-completion-map
							minibuffer-local-must-match-map
							minibuffer-local-isearch-map
							minibuffer-inactive-mode-map
							)
	 [escape] 'minibuffer-keyboard-quit
	 "C-w" 'evil-delete-backward-word
	 )

	(global-set-key [escape] 'keyboard-quit) ;;evil-exit-emacs-state


	(define-key ctl-x-map (kbd "C-j") 'delete-blank-lines)

	(general-create-definer mikus-leader
		:prefix "SPC")

	(general-define-key
	 :keymaps '( compilation-mode-map compilation-minor-mode-map )
	 ;; :states '(motion normal)
	 "C-n" 'compilation-next-error
	 "C-p" 'compilation-previous-error
	 )

	(general-define-key
	 "C-s" 'my/evil-line-substitute
	 "C-S-s" 'my/evil-global-substitute)


	(mikus-leader
		:states '(normal motion visual)
		:keymaps 'override
		"ci" 'evilnc-comment-or-uncomment-lines
		"cl" 'evilnc-quick-comment-or-uncomment-to-the-line
		"ll" 'evilnc-quick-comment-or-uncomment-to-the-line
		"cc" 'evilnc-copy-and-comment-lines
		"cp" 'evilnc-comment-or-uncomment-paragraphs
		"cr" 'comment-or-uncomment-region
		"cv" 'evilnc-toggle-invert-comment-line-by-line
		"."  'evilnc-copy-and-comment-operator
		"\\" 'evilnc-comment-operator ; if you prefer backslash key
		"w"  (lambda () (interactive) (evil-without-repeat (call-interactively #'hydra-window/body)))
		"p"  'projectile-command-map
		"a"  'ace-window
		"k"  'kill-buffer
		"g"	 'mikus-magit-map
		"ef" 'flycheck-buffer
		"el" 'flycheck-list-errors
		"en" 'flycheck-next-error
		"ep" 'flycheck-previous-error
		"r"  'evil-use-register
		"<SPC>" 'whitespace-cleanup
		"<tab>" 'switch-to-recently-selected-buffer
		)

	(mikus-leader
		:states '(normal motion visual)
		:keymaps 'override
		"jf" 'evil-jump-forward
		"jb" 'evil-jump-backward
		"js" 'evil-jump-backward-swap
		)

	(general-define-key

	 :states '(motion normal)

	 [escape] 'keyboard-quit

	 "C-;"  (lambda () (interactive) (evil-scroll-column-right 5))
	 "C-j"  (lambda () (interactive) (evil-scroll-column-left 5))
	 "M-;"  (lambda () (interactive) (evil-scroll-column-right 5))
	 "M-v" 'evil-visual-line

	 "k" 'evil-next-line
	 "l" 'evil-previous-line
	 "`" 'evil-paste-from-register
	 ";" 'evil-forward-char
	 "j" 'evil-backward-char
	 "h" 'evil-goto-mark
	 "C-f" nil
	 "C-k"  (lambda () (interactive) (evil-scroll-line-down 3))
	 "C-l"  (lambda () (interactive) (evil-scroll-line-up 3))
	 "C-S-k" 'evil-scroll-page-down
	 "C-S-l" 'evil-scroll-page-up
	 "C-y" nil
	 "C-e" #'evil-last-non-blank
	 "ge" #'go-end-of-visual-line
	 "gb" #'go-start-of-visual-line
	 "C-b" 'evil-first-non-blank
	 "M-b" #'backward-word
	 "}" 'evil-repeat-find-char
	 "{" 'evil-repeat-find-char-reverse
	 "<" 'evil-jump-backward
	 ">" 'evil-jump-forward

	 "<C-tab>" 'switch-to-the-window-that-displays-the-most-recently-selected-buffer

	 "M-." nil
	 "M-," nil
	 "g'" 'goto-last-change
	 "g," 'goto-last-change-reverse
	 "^" 'evil-ex-repeat-substitute
	 "g^" 'evil-ex-repeat-global-substitute
	 "\'" 'evil-repeat-find-char

	 ;; avy
	 "s" 'evil-avy-goto-word-or-subword-1
	 "M-s" 'evil-avy-goto-word-1-in-line
	 "M-h" #'avy-pop-mark
	 "M-y" #'avy-copy-line
	 "\'" 'evil-avy-goto-char
	 "M-\'" 'evil-avy-goto-char-in-line
	 "("  'evil-avy-goto-paren-left
	 ")"  'evil-avy-goto-paren-right

	 )

	(general-define-key
	 :states '(motion insert)
	 "M-o" 'insert-line-below
	 "M-O" 'insert-line-above
	 "M-J" 'evil-join-and-indent-upwards
	 "M-j" 'evil-join-and-indent
	 "M-i" 'evil-enter-insert-and-intent
	 "C-b" nil
	 "C-p" nil
	 "C-o" nil
	 )

	(general-define-key
	 :states 'insert
	 "M-k" 'next-line
	 "M-l" 'previous-line
	 "C-k" 'next-line
	 "C-l" 'previous-line
	 "C-j" 'backward-char
	 "C-;" 'forward-char
	 "C-n" 'evil-complete-next
	 "C-y" nil
	 "<escape>" 'evil-normal-state
	 "C-." 'yas-expand
	 )
	
	(general-define-key
	 :states '(motion visual)
	 "C-M-k"  'evil-avy-goto-line-below
	 "C-M-l"  'evil-avy-goto-line-above
	 "K"  'evil-next-visual-line
	 "L"  'evil-previous-visual-line
	 "gK" 'evil-window-bottom
	 "gL" 'evil-window-top
	 "M-k"  'forward-paragraph
	 "M-l"  'backward-paragraph
	 "M-n" 'drag-stuff-down
	 "M-p" 'drag-stuff-up
	 "M-f" 'drag-stuff-right
	 "M-b" 'drag-stuff-left
	 )


	;;; WINDOW

	(general-define-key
	 :keymaps 'evil-window-map
	 "j" 'evil-window-left
	 "J" 'evil-window-move-far-left
	 "k" 'evil-window-down
	 "K" 'evil-window-move-very-bottom
	 "l" 'evil-window-up
	 "L" 'evil-window-move-very-top
	 ";" 'evil-window-right
	 ":" 'evil-window-move-far-right
	 "|" (lambda ()
				 (interactive)
				 (split-window-right)
				 (windmove-right))
	 "_" (lambda ()
				 (interactive)
				 (split-window-below)
				 (windmove-down))
	 )



	(define-key evil-motion-state-map (kbd "C-z") nil) ;turn off this switch and we will remap this
	(define-key evil-emacs-state-map (kbd "C-z") nil) ;turn off this switch and we will remap this
	(global-set-key (kbd "<C-f8>") 'my/evil-switch-emacs-state) ; here we map this to f8


	(global-unset-key (kbd "C-SPC"))
	;; (global-set-key (kbd "C-SPC") 'company-complete-common)

	)

(use-package evil-visualstar
	:ensure t
	:init
	(global-evil-visualstar-mode))




(provide 'init-evil)
