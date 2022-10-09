;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.

;;; Code
(use-package eldoc
  :commands (eldoc-mode)
  :hook (prog-mode . eldoc-mode)
  :config
  ;; Original code from
  ;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
  ;; thanks!
  ;; (radian-defadvice radian--advice-disable-eldoc-on-flycheck
  ;;     (&rest _)
  ;;   :after-while eldoc-display-message-no-interference-p
  ;;   "Disable ElDoc when point is on a Flycheck overlay.
  ;; 			This prevents ElDoc and Flycheck from fighting over the echo
  ;; 			area."
  ;;   (not (and (bound-and-true-p flycheck-mode)
  ;; 	      (flycheck-overlay-errors-at (point)))))
  (global-eldoc-mode 0)

  (defun turn-on-eldoc-mode ())

  (defun eldoc-print ()
    "command to trigger eldoc, add that to 'eldoc-message-commands"
    (interactive)
    (eldoc-print-current-symbol-info))
  (eldoc-add-command 'eldoc-print)
  (mikus-leader "h" 'eldoc-print)
  (setq eldoc-documentation-function
	(lambda ()
	  (when (eql last-command-event 32)
	    (let (eldoc-documentation-function)
	      (eldoc-print-current-symbol-info)))))
  (setq eldoc-echo-area-use-multiline-p t))

(provide 'init-eldoc)
