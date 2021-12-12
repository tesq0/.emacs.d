(defun evil-maybe-make-normal-state-pattern ()
  (when (evil-normal-state-p)
    (let* ((bounds (bounds-of-thing-at-point 'word))
	   (start (car bounds))
	   (end (cdr bounds))
	   (range (evil-range start end))
	   (evil-ex-search-case 'sensitive))
      (goto-char (- end 1))
      (evil-mc-set-pattern-for-range range nil))))

(use-package evil-mc
  :commands (global-evil-mc-mode)
  :init
  (global-evil-mc-mode)
  :config
  (setq mc/always-run-for-all t)
  (setq evil-mc-one-cursor-show-mode-line-text t)
  (general-define-key
   :states '(motion normal visual)
   "C-n" 'evil-mc-make-and-goto-next-match
   "C-S-n" 'evil-mc-skip-and-goto-next-match
   "C-p" 'evil-mc-make-and-goto-prev-match
   "C-S-p" 'evil-mc-skip-and-goto-prev-match)

  ;; by default make the patter current word
  (mapcar (lambda (symbol) (advice-add symbol :before #'evil-maybe-make-normal-state-pattern ))
	  '(evil-mc-make-and-goto-next-match evil-mc-make-and-goto-prev-match))

  (setq evil-mc-enable-bar-cursor nil)
  (general-define-key
   :states '(motion normal visual)
   :keymaps 'evil-mc-key-map
   "M-n" nil
   "M-p" nil
   "C-n" nil
   "C-S-n" nil
   "C-p"   nil
   "C-S-p" nil))

;; (advice-mapc (lambda (advice props) (message (format "fun %s" advice))) 'evil-mc-make-and-goto-next-match)
;; (advice-mapc (lambda (advice props) (message (format "fun %s" advice))) 'evil-mc-make-and-goto-prev-match)

;; (mapcar (lambda (symbol) (advice-mapc #'(lambda (advice props) (general-remove-advice symbol advice)) symbol))
;; 				'(evil-mc-make-and-goto-next-match evil-mc-make-and-goto-prev-match))


(provide 'init-mc)
