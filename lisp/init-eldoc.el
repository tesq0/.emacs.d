;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.

;;; Code
(use-package eldoc
  :demand
  :hook (prog-mode . turn-eldoc-mode-on)
  :config
  ;; Original code from
  ;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
  ;; thanks!
  (radian-defadvice radian--advice-disable-eldoc-on-flycheck
      (&rest _)
    :after-while eldoc-display-message-no-interference-p
    "Disable ElDoc when point is on a Flycheck overlay.
			This prevents ElDoc and Flycheck from fighting over the echo
			area."
    (not (and (bound-and-true-p flycheck-mode)
	      (flycheck-overlay-errors-at (point)))))
  (defun eldoc-print ()
    "command to trigger eldoc, add that to 'eldoc-message-commands"
    (interactive))
  (eldoc-add-command 'eldoc-print)
  (mikus-leader "h" 'eldoc-print)
  (setq eldoc-echo-area-use-multiline-p t))

(provide 'init-eldoc)
