(setq package-enable-at-startup nil)

;; (setq package-archives '(("org"       . "https://orgmode.org/elpa/")
;; 			 ("gnu"       . "https://elpa.gnu.org/packages/")
;; 			 ("melpa"     . "https://melpa.org/packages/")))
;; Initialize packages
;; (package-initialize)

;; Setup `use-package'
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; Should set before loading `use-package'
;; (defvar use-package-always-ensure t)
(defvar use-package-always-defer t)
(defvar use-package-expand-minimally t)
(defvar use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
