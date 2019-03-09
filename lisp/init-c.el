;; C,C++
(use-package irony
	:ensure t
	:init
	(progn
		(setq irony--server-executable (concat (getenv "HOME") "/.nix-profile/bin/irony-server"))
		(add-hook 'c++-mode-hook 'irony-mode)
		(add-hook 'c-mode-hook 'irony-mode)
		(add-hook 'objc-mode-hook 'irony-mode)
		(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package flycheck-irony
	:after irony
	:ensure t
	:init
	(flycheck-irony-setup))

(use-package irony-eldoc
	:after irony
	:ensure t)

(after-load 'cc-mode
	(init-c-style)
	(add-hook 'c-mode-hook #'irony-eldoc))

(defun irony-iotask-ectx-call-callback (ectx result)
  (let ((cb-buffer (irony-iotask-ectx-schedule-buffer ectx)))
    (when (buffer-live-p cb-buffer)
      (with-demoted-errors "Irony I/O task: error in callback: %S"
        (with-current-buffer cb-buffer
          (funcall (irony-iotask-ectx-callback ectx) result))))))

(defun init-c-style()
	"Define my own indenting style for C."
	(c-add-style "mikus-c"
							 '("gnu"
								 (c-basic-offset . 4)
								 (c-offsets-alist
									(innamespace . +)
									(template-args-cont . +)
									(inline-open . 0)
									(case-label . +)
									(inher-cont . c-lineup-multi-inher)
									(arglist-cont-nonempty . 0)
									(arglist-intro . +)
									(arglist-close . c-lineup-close-paren))))
	(add-to-list 'c-default-style '(c-mode . "mikus-c")))


(provide 'init-c)
