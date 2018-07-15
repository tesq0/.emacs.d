(use-package evil
	:ensure t
	:config
	(evil-set-initial-state 'term-mode 'emacs)
	(evil-set-initial-state 'calendar-mode 'emacs)
	(evil-set-initial-state 'magit-mode 'emacs)
	(evil-set-initial-state 'image-mode 'emacs)
	(setq initial-major-mode 'evil-mode)                 ; set the mode of the initial scratch buffer
	;; (setq evil-emacs-state-cursor '("#B22222" box))
	;; (setq evil-normal-state-cursor '("green" box))
	;; (setq evil-visual-state-cursor '("purple" box))
	;; (setq evil-insert-state-cursor '("orange" bar))
	;; (setq evil-replace-state-cursor '("red" bar))
	;; (setq evil-operator-state-cursor '("red" hollow))
	;; :init
	;; (add-hook 'indium-repl-mode-hook 'evil-mode nil)
	)


(use-package evil-leader
	:ensure t)

(use-package avy
	:ensure t
	)

(use-package evil-surround
	:ensure t
	:init
	(global-evil-surround-mode))


(use-package evil-nerd-commenter
	:ensure t)

;; helm-split-window-inside-p           t

;; text manipulation.


(use-package linum-relative
	:ensure t
	;; :config
	;; (setq linum-relative-backend 'display-line-numbers-mode)
	)



;;; Start evil
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
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
	"g"  'magit-status
	"p"  'projectile-command-map
	"a"  'ace-window
	;;"f"  (lambda () (interactive) (evil-without-repeat (call-interactively #'hydra-fzf/body)))
	"k"  'kill-buffer
	"O"  'projectile-multi-occur
	"ef" 'flycheck-buffer
	"el" 'flycheck-list-errors
	"en" 'flycheck-next-error
	"ep" 'flycheck-previous-error
	"jf" 'evil-jump-forward
	"jb" 'evil-jump-backward
	"js" 'evil-jump-backward-swap
	"r"  'evil-use-register
	"<SPC>" 'whitespace-cleanup

	
	"b" 'ivy-switch-buffer
	"f" 'counsel-find-file
	)

(evil-mode 1)

(defun my/evil-switch-emacs-state ()
	"Switch between Emacs and evil state."
	(interactive)
	(if (evil-emacs-state-p)
			(evil-exit-emacs-state)
		(evil-emacs-state)))


(defun my/make-newline-before (times)
	"Insert newline before TIMES."
	(interactive "p")
	(save-excursion
		(move-beginning-of-line 1)
		(newline times)))

(defun my/make-newline-after (times)
	"Insert newline before TIMES."
	(interactive "p")
	(save-excursion
		(move-end-of-line 1)
		(newline times)))

(use-package drag-stuff
	:ensure t
	)



(with-eval-after-load 'evil-maps



	
	(define-key evil-normal-state-map (kbd "M-n") 'drag-stuff-down)
	(define-key evil-normal-state-map (kbd "M-p") 'drag-stuff-up)
	(define-key evil-normal-state-map (kbd "M-f") 'drag-stuff-right)
	(define-key evil-normal-state-map (kbd "M-b") 'drag-stuff-left)
	(define-key evil-visual-state-map (kbd "M-n") 'drag-stuff-down)
	(define-key evil-visual-state-map (kbd "M-p") 'drag-stuff-up)
	(define-key evil-visual-state-map (kbd "M-f") 'drag-stuff-right)
	(define-key evil-visual-state-map (kbd "M-b") 'drag-stuff-left)

	(global-set-key (kbd "C-S-v") 'evil-paste-after)
	(global-set-key (kbd "C-S-c") 'evil-yank)


	;;; Motion
	(evil-define-motion move-2-lines-down ()
		(evil-next-visual-line 2))
	(evil-define-motion move-2-lines-up ()
		(evil-previous-visual-line 2))

	(global-set-key (kbd "<f9>") 'repeat-complex-command)

	(define-key evil-motion-state-map (kbd "C-;")  (lambda () (interactive) (evil-scroll-column-right 20)))
	(define-key evil-motion-state-map (kbd "C-j")  (lambda () (interactive) (evil-scroll-column-left 20)))
	(define-key evil-motion-state-map (kbd "M-;")  (lambda () (interactive) (evil-scroll-column-right 20)))
	(define-key evil-motion-state-map (kbd "M-v") 'evil-visual-line)

	(define-key evil-motion-state-map "k" 'evil-next-visual-line)
	(define-key evil-motion-state-map "l" 'evil-previous-visual-line)
	(define-key evil-motion-state-map "`" 'evil-paste-from-register)
	(define-key evil-motion-state-map ";" 'evil-forward-char)
	(define-key evil-motion-state-map "j" 'evil-backward-char)
	(define-key evil-motion-state-map "h" 'evil-goto-mark)
	(define-key evil-motion-state-map (kbd "'") 'evil-repeat-find-char)
	(define-key evil-motion-state-map (kbd "C-f") nil) ; Disable the C-F, i'll use it as a prefix
	(define-key evil-motion-state-map (kbd "C-k")  (lambda () (interactive) (evil-scroll-line-down 3)))
	(define-key evil-motion-state-map (kbd "C-l")  (lambda () (interactive) (evil-scroll-line-up 3)))
	(define-key evil-motion-state-map (kbd "C-e")  (lambda () (interactive) (evil-scroll-line-down 3)))

	(define-key evil-motion-state-map (kbd "C-y") nil)
	(define-key evil-insert-state-map (kbd "C-y") nil)



	;; (define-key evil-motion-state-map (kbd "<C-tab>") 'evil-jump-backward)
	;; (define-key evil-motion-state-map (kbd "<tab>") 'evil-jump-forward)
	(global-set-key (kbd "M-J") 'join-line)
	(global-set-key (kbd "M-j") 'evil-join)
	(define-key ctl-x-map (kbd "C-j") 'delete-blank-lines)

	(define-key evil-motion-state-map (kbd "M-o") 'my/make-newline-after)
	(define-key evil-motion-state-map (kbd "M-O") 'my/make-newline-before)
	(define-key evil-motion-state-map (kbd "C-o") nil)


	;; (define-key evil-motion-state-map (kbd "K")  'forward-paragraph)
	;; (define-key evil-motion-state-map (kbd "L")  'backward-paragraph)
	;; (define-key evil-visual-state-map (kbd "K")  'forward-paragraph)
	;; (define-key evil-visual-state-map (kbd "L")  'backward-paragraph)
	(define-key evil-motion-state-map (kbd "C-M-k")  'evil-avy-goto-line-below)
	(define-key evil-motion-state-map (kbd "C-M-l")  'evil-avy-goto-line-above)
	(define-key evil-visual-state-map (kbd "C-M-k")  'evil-avy-goto-line-below)
	(define-key evil-visual-state-map (kbd "C-M-l")  'evil-avy-goto-line-above)

	(define-key evil-motion-state-map (kbd "K")  'forward-paragraph)
	(define-key evil-motion-state-map (kbd "L")  'backward-paragraph)
	(define-key evil-visual-state-map (kbd "K")  'forward-paragraph)
	(define-key evil-visual-state-map (kbd "L")  'backward-paragraph)
	(define-key evil-motion-state-map (kbd "M-k")  'forward-paragraph)
	(define-key evil-motion-state-map (kbd "M-l")  'backward-paragraph)
	(define-key evil-visual-state-map (kbd "M-k")  'forward-paragraph)
	(define-key evil-visual-state-map (kbd "M-l")  'backward-paragraph)


	(define-key evil-insert-state-map (kbd "C-\'") 'yas-expand)

	(define-key evil-motion-state-map "&" 'evil-end-of-line)
	(define-key evil-normal-state-map "&" 'evil-end-of-line)
	(define-key evil-visual-state-map "&" 'evil-end-of-line)

	(define-key	evil-normal-state-map (kbd "C-n") nil)
	(define-key	evil-normal-state-map (kbd "C-p") nil)
	(define-key evil-motion-state-map (kbd "C-b") nil)
	(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
	(define-key evil-motion-state-map "&" 'evil-end-of-line)
	(define-key evil-normal-state-map "&" 'evil-end-of-line)
	(define-key evil-visual-state-map "&" 'evil-end-of-line)


	(define-key evil-motion-state-map "g&" 'evil-end-of-visual-line)
	(define-key evil-normal-state-map "g&" 'evil-end-of-visual-line)
	(define-key evil-visual-state-map "g&" 'evil-end-of-visual-line)

	(define-key evil-motion-state-map "$" 'evil-beginning-of-line)
	(define-key evil-motion-state-map "g&" 'evil-end-of-visual-line)
	(define-key evil-motion-state-map "g$" 'evil-beginning-of-visual-line)
	(define-key evil-normal-state-map "g&" 'evil-end-of-visual-line)
	(define-key evil-normal-state-map "g$" 'evil-beginning-of-visual-line)
	(define-key evil-motion-state-map (kbd "M-;") 'evil-ex)


	;;; WINDOW
	(define-key evil-window-map "j" 'evil-window-left)
	(define-key evil-window-map "J" 'evil-window-move-far-left)
	(define-key evil-window-map "k" 'evil-window-down)
	(define-key evil-window-map "K" 'evil-window-move-very-bottom)
	(define-key evil-window-map "l" 'evil-window-up)
	(define-key evil-window-map "L" 'evil-window-move-very-top)
	(define-key evil-window-map ";" 'evil-window-right)
	(define-key evil-window-map ":" 'evil-window-move-far-right)
	(general-define-key
	 :keymaps 'evil-window-map
	 "|" (lambda ()
					(interactive)
					(split-window-right)
					(windmove-right))
   "_" (lambda ()
					(interactive)
					(split-window-below)
					(windmove-down))
	 )




	;;; Insert mode

	(define-key evil-motion-state-map (kbd "C-z") nil) ;turn off this switch and we will remap this
	(define-key evil-emacs-state-map (kbd "C-z") nil) ;turn off this switch and we will remap this
	(global-set-key (kbd "<C-f8>") 'my/evil-switch-emacs-state) ; here we map this to f8



	(define-key evil-insert-state-map (kbd "C-i") 'self-insert-command)
	(global-unset-key (kbd "C-SPC"))



	(define-key evil-insert-state-map (kbd "C-p") 'yank)

	(global-set-key (kbd "C-SPC") 'company-complete-common)


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

	(define-key evil-motion-state-map   (kbd "C-e") #'go-end-of-line)
	(define-key evil-motion-state-map   (kbd "ge") #'go-end-of-visual-line)

	(define-key evil-motion-state-map   (kbd "gb") #'go-start-of-visual-line)
	(define-key evil-motion-state-map   (kbd "C-b") #'go-start-of-line)

	;; (define-key evil-motion-state-map   (kbd "M-b") #'backward-word)

	(define-key evil-motion-state-map   (kbd "M-h") #'avy-pop-mark)
	(define-key evil-motion-state-map   (kbd "M-y") #'avy-copy-line)

	;; esc quits
	(defun minibuffer-keyboard-quit ()
		"Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
		(interactive)
		(if (and delete-selection-mode transient-mark-mode mark-active)
				(setq deactivate-mark  t)
			(when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
			(abort-recursive-edit)))
	(define-key evil-normal-state-map [escape] 'keyboard-quit)
	(define-key evil-visual-state-map [escape] 'keyboard-quit)
	(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
	(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
	(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
	(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
	(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
	(global-set-key [escape] 'keyboard-quit) ;;evil-exit-emacs-state
	(define-key evil-normal-state-map (kbd "M-.") nil)
	(define-key evil-normal-state-map (kbd "M-,") nil)
	(define-key evil-normal-state-map "g'" 'goto-last-change)
	(define-key evil-normal-state-map "g," 'goto-last-change-reverse)
	(define-key evil-normal-state-map "^" 'evil-ex-repeat-substitute)
	(define-key evil-normal-state-map "g^" 'evil-ex-repeat-global-substitute)
	;; (define-key evil-normal-state-map "\"" 'evil-ex)

	;;; avy
	(define-key evil-normal-state-map "\'" 'evil-repeat-find-char)
	(define-key evil-normal-state-map "s" 'evil-avy-goto-word-or-subword-1)
	(define-key evil-normal-state-map "S" 'evil-avy-goto-char)

	(global-set-key (kbd "M-s") 'evil-avy-goto-char-in-line)



	)


(global-evil-visualstar-mode)

(defun my-move-key (keymap-from keymap-to key)
	"Moves key binding from one keymap to another, deleting from the old location. "
	(define-key keymap-to key (lookup-key keymap-from key))
	(define-key keymap-from key nil))
;; (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

;; Magit integration
;; (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)


(provide 'init-evil)
