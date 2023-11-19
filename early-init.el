;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; gc 优化启动速度
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
