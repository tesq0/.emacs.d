(autoload 'ggtags "ggtags-mode")

(with-eval-after-load 'ggtags
  (setenv "GTAGSLABEL" "pygments")
  (setq ggtags-highlight-tag nil
	ggtags-split-window-function nil
	ggtags-global-window-height nil
	ggtags-completing-read-function 'nil
	ggtags-auto-jump-to-match 'first)

  (defun compilation-maybe-halt-auto-jump (buffer pos)
    "Halt jumping to first match in ggtags-global-mode if more that 1 results."
    (let* ((bname (buffer-name buffer))
	   (ggtags (string-equal bname "*ggtags-global*")))
      (when ggtags
	(with-current-buffer buffer
	  (let* ((lines (count-lines pos (point-max)))
		 (halt (> lines 4))) ;; more than 4 seems to mean more than 1 match
	    ;; (message (format "output lines %s halt? %s" lines halt))
	    (when halt
	      (setq compilation-auto-jump-to-first-error nil)))))))

  (defun ggtags-query-tags (name)
    (interactive (list (ggtags-read-tag 'definition 1)))
    (ggtags-find-tag 'definition "--" (shell-quote-argument name)))

  (defun ggtags-eldoc-function ())

  (advice-add 'compilation-auto-jump :before #'compilation-maybe-halt-auto-jump)

  (define-key ggtags-global-mode-map "C-c C-p" 'wgrep-change-to-wgrep-mode)
  (define-key ggtags-global-mode-map "C-c C-r" 'nil)


  (define-key miko-gtags-map "P" 'ggtags-visit-project-root)
  (define-key miko-gtags-map "h" 'ggtags-view-tag-history)
  (define-key miko-gtags-map "f" 'ggtags-find-file)
  (define-key miko-gtags-map "g" 'ggtags-grep)
  (define-key miko-gtags-map "s" 'ggtags-find-other-symbol)
  (define-key miko-gtags-map "r" 'ggtags-find-reference)
  (define-key miko-gtags-map "`" 'ggtags-save-to-register)
  (define-key miko-gtags-map "t" 'ggtags-query-tags)
  (define-key miko-gtags-map "q" 'ggtags-query-replace)
  (define-key miko-gtags-map "n" 'ggtags-next-mark)
  (define-key miko-gtags-map "p" 'ggtags-prev-mark)
  (define-key miko-gtags-map "d" 'ggtags-show-definition)

  (global-set-key (kbd "C-c t") 'miko-gtags-map))


(provide 'init-tags)
