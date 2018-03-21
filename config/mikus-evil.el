

(use-package evil
  :ensure t)

(use-package evil-leader
  :ensure t)

(use-package avy
  :ensure t
	)



(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))


(use-package evil-nerd-commenter
  :ensure t)

(use-package evil-matchit
  :ensure t
	:init
	(global-evil-matchit-mode 1) 
	)

;; (use-package evil-mc
;;   :ensure t
;;   :config
;;   (global-evil-mc-mode 1)
;;   )


;; helm-split-window-inside-p           t
(use-package helm
	:ensure t 
	:config 
	(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
	(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
	(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
	(setq 
	helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
	helm-split-window-inside-p t ; open helm buffer inside current window, not occupy whole other window
	helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line t) 
	(setq helm-autoresize-max-height 0)
	(setq helm-autoresize-min-height 20)
	(helm-autoresize-mode 1)
	(setq helm-buffers-fuzzy-matching t
				helm-recentf-fuzzy-match    t)
	(helm-mode 1)

	) 


(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)



(use-package helm-projectile
	:ensure t 
	)
(use-package evil-visualstar
  :ensure t)




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
  "b"  'helm-buffers-list
  "p"  'helm-projectile-find-file
  "k"  'helm-register
  "a"  'ace-window
  ;;"f"  (lambda () (interactive) (evil-without-repeat (call-interactively #'hydra-fzf/body)))
	"f"  'fzf-directory
	"s"  'evil-avy-goto-char
	"k"  'kill-buffer
	"q"  'helm-show-kill-ring
  "m"  'helm-all-mark-rings
  "o"  'helm-occur
  "t"  'helm-top
  "x"  'helm-M-x
  "ef" 'flycheck-buffer
  "el" 'flycheck-list-errors
  "en" 'flycheck-next-error
  "ep" 'flycheck-previous-error
	"r"  'evil-use-register

  ) 

(evil-mode 1)

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
	(transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))


(with-eval-after-load 'evil-maps
  ;;; Motion
	(evil-define-motion move-2-lines-down ()
  (evil-next-visual-line 2))
	(evil-define-motion move-2-lines-up ()
  (evil-previous-visual-line 2))

  ;; (define-key evil-motion-state-map (kbd "C-;")  (lambda () (interactive) (evil-scroll-column-right 20) (evil-forward-char 20)))
  ;; (define-key evil-motion-state-map (kbd "C-j")  (lambda () (interactive) (evil-scroll-column-left 20) (evil-backward-char 20)))
	
  (define-key evil-motion-state-map (kbd "C-;")  (lambda () (interactive) (evil-scroll-column-right 20)))
  (define-key evil-motion-state-map (kbd "C-j")  (lambda () (interactive) (evil-scroll-column-left 20)))
  (define-key evil-motion-state-map (kbd "M-;")  (lambda () (interactive) (evil-scroll-column-right 20)))
  (define-key evil-motion-state-map (kbd "M-j")  (lambda () (interactive) (evil-scroll-column-left 20)))
  (define-key evil-motion-state-map "k" 'evil-next-visual-line)
  (define-key evil-motion-state-map "l" 'evil-previous-visual-line)
  (define-key evil-motion-state-map "`" 'evil-paste-from-register) 
  (define-key evil-motion-state-map ";" 'evil-forward-char)
  (define-key evil-motion-state-map "j" 'evil-backward-char)
  (define-key evil-motion-state-map "h" 'evil-goto-mark)
  (define-key evil-motion-state-map (kbd "'") 'evil-repeat-find-char)
  ;; (define-key evil-motion-state-map (kbd "C-k")  (lambda () (interactive) (evil-scroll-line-down 3) (evil-next-visual-line 3)))
  ;; (define-key evil-motion-state-map (kbd "C-l")  (lambda () (interactive) (evil-scroll-line-up 3) (evil-previous-visual-line 3))) 
  (define-key evil-motion-state-map (kbd "C-k")  (lambda () (interactive) (evil-scroll-line-down 3)))
  (define-key evil-motion-state-map (kbd "C-l")  (lambda () (interactive) (evil-scroll-line-up 3))) 
  (define-key evil-motion-state-map (kbd "M-k")  (lambda () (interactive) (evil-scroll-line-down 3)))
  (define-key evil-motion-state-map (kbd "M-l")  (lambda () (interactive) (evil-scroll-line-up 3))) 
  (define-key evil-motion-state-map (kbd "C-e")  (lambda () (interactive) (evil-scroll-line-down 3)))
  (define-key evil-motion-state-map (kbd "C-y")  (lambda () (interactive) (evil-scroll-line-up 3))) 
  (define-key evil-motion-state-map (kbd "K")  'evil-avy-goto-line-below)
  (define-key evil-motion-state-map (kbd "L")  'evil-avy-goto-line-above) 


	;; same for visual 
  (define-key evil-visual-state-map (kbd "K")  'move-2-lines-down)
  (define-key evil-visual-state-map (kbd "L")  'move-2-lines-up) 
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


  ;;; WINDOW
  (define-key evil-window-map "j" 'evil-window-left)
  (define-key evil-window-map "J" 'evil-window-move-far-left)
  (define-key evil-window-map "k" 'evil-window-down)
  (define-key evil-window-map "K" 'evil-window-move-very-bottom)
  (define-key evil-window-map "l" 'evil-window-up)
  (define-key evil-window-map "L" 'evil-window-move-very-top)
  (define-key evil-window-map ";" 'evil-window-right)
  (define-key evil-window-map ":" 'evil-window-move-far-right)


	;;; Insert mode 

	(define-key evil-insert-state-map "\C-i" 'self-insert-command) 
  (define-key evil-insert-state-map (kbd "C-n") 'company-complete-common-or-cycle)
	(define-key evil-insert-state-map (kbd "C-p")  'company-select-previous)


  ;;; Normal mode 
  (define-key evil-normal-state-map (kbd "M-.") 'tern-find-definition)
  (define-key evil-normal-state-map (kbd "M-,") 'tern-pop-find-definition)
  (define-key evil-normal-state-map "g;" nil)
  (define-key evil-normal-state-map "g'" 'goto-last-change)
  (define-key evil-normal-state-map "g," 'goto-last-change-reverse)
  (define-key evil-normal-state-map "^" 'evil-ex-repeat-substitute)
  (define-key evil-normal-state-map "g^" 'evil-ex-repeat-global-substitute)
  ;; (define-key evil-normal-state-map "\"" 'evil-ex)

	;;; avy
  (define-key evil-normal-state-map "\'" 'evil-repeat-find-char)
  (define-key evil-normal-state-map "s" 'evil-avy-goto-word-or-subword-1)

	;;; Visual mode 
  (define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
  (define-key evil-normal-state-map (kbd "] e") 'move-text-down)
  (define-key evil-visual-state-map (kbd "[ e") ":move'<--1")
  (define-key evil-visual-state-map (kbd "] e") ":move'>+1")



  ) 


(global-evil-visualstar-mode)

(defun my-move-key (keymap-from keymap-to key)
	"Moves key binding from one keymap to another, deleting from the old location. "
	(define-key keymap-to key (lookup-key keymap-from key))
	(define-key keymap-from key nil))
;; (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

;; Magit integration
(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)


(provide 'mikus-evil)
