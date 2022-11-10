(define-prefix-command 'mikus-search-map)

(global-set-key (kbd "C-c s") 'mikus-search-map)

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-w") 'isearch-delete-char)
  (define-key isearch-mode-map (kbd "C-g") 'isearch-exit))

;; (load 'fzf)

;; (defun fzf ()
;;     "Starts a fzf session."
;;     (interactive)
;;     (if (fboundp #'projectile-project-root)
;; 	(fzf/start (condition-case err
;; 		       (or (projectile-project-root) default-directory)
;; 		     (error
;; 		      default-directory)))
;;       (fzf/start default-directory)))

;;   (defun fcd ()
;;     (interactive)
;;     (fzf/start "" (format "fd -t d %s" default-directory)))

;; (define-key mikus-search-map "f" 'fzf)

(autoload 'rg "rg")
(autoload 'rg-project "rg")
(autoload 'rg-dwim-current-dir "rg")
(autoload 'rg-dwim-project-dir "rg")

(define-key mikus-search-map "d" 'rg-dwim-current-dir)
(define-key mikus-search-map "r" 'rg)
(define-key mikus-search-map "g" 'find-grep)
(define-key mikus-search-map "p" 'rg-dwim-project-dir)
(define-key mikus-search-map "P" 'rg-project)

(with-eval-after-load 'rg
  (defun rg-reload ()
    (interactive)
    (rg-rerun))

  (defvar rg-cur-regexp "Regexp of rg's current search pattern" nil)
  (defvar rg-opt-multiline "Should add the --multiline option?" nil)

  (defun rg-command-line-flags ()
    (and rg-opt-multiline (list "--multiline")))


  (defun rg-rerun-toggle-multiline ()
    (interactive)
    (setq-local rg-opt-multiline (not rg-opt-multiline))
    (message "rg-opt-multiline %s" rg-opt-multiline)
    (rg-rerun))

  (setq rg-hide-command nil)

  (rg-define-search rg-project-merge-conflicts
		    :dir project
		    :query "<<<<<<<"
		    :files current)

  (define-key 'mikus-search-map "m" 'rg-project-merge-conflicts)

  (define-key rg-mode-map "r" 'rg-rerun-change-regexp)
  (define-key rg-mode-map "m" 'rg-rerun-toggle-multiline)
  (define-key rg-mode-map "f" 'rg-rerun-change-files)
  (define-key rg-mode-map "d" 'rg-rerun-change-dir)
  (define-key rg-mode-map "C-c C-p" 'wgrep-change-to-wgrep-mode)
  (define-key rg-mode-map "C-c C-r" 'rg-reload)
  
  (setq rg-group-result nil))

(provide 'init-search)
