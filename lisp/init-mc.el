(use-package evil-mc
	:ensure t
	:init
	(progn
		(setq mc/always-run-for-all t)
		(setq evil-mc-enable-bar-cursor nil)
		(setq evil-mc-one-cursor-show-mode-line-text t)
		(general-define-key
		 :states '(motion normal visual)
		 "C-n" 'evil-mc-make-and-goto-next-match
		 "C-S-n" 'evil-mc-skip-and-goto-next-match
		 "C-p" 'evil-mc-make-and-goto-prev-match
		 "C-S-p" 'evil-mc-skip-and-goto-prev-match)

		 ;; by default make the patter current word
		 (mapcar (lambda (name) (advice-add (intern name) :before #'(lambda () (if (evil-normal-state-p) (setq-local evil-mc-pattern (cons (evil-mc-make-pattern (word-at-point) nil) (evil-visual-range)))))))
						 (list "evil-mc-make-and-goto-next-match" "evil-mc-make-and-goto-next-match"))
		 ;; (mapcar (lambda (name) (advice-add (intern name) :after #'(lambda () (if (evil-normal-state-p) (evil-backward-word-begin)))))
		 ;; 				 (list "evil-mc-make-and-goto-next-match" "evil-mc-make-and-goto-next-match"))
		 (global-evil-mc-mode t))
	:config
	(general-define-key
	 :states '(motion normal visual)
	 :keymaps 'evil-mc-key-map
	 "M-n" nil
	 "M-p" nil
	 "C-n" nil
	 "C-S-n" nil
	 "C-p"   nil
	 "C-S-p" nil)
	)
	
	;; (mapcar (lambda (name) (advice-mapc #'(lambda (advice props) (general-remove-advice (intern name) advice)) (intern name)))
	;; 				(list "evil-mc-make-and-goto-next-match" "evil-mc-make-and-goto-next-match"))


(provide 'init-mc)
