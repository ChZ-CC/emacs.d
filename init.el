;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;; Tidy workdir
(make-directory "~/.emacs.d/data/backup/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/data/backup/" t)) ; Write auto-save files to a separate directory
      backup-directory-alist '(("." . "~/.emacs.d/data/backup/"))          ; Write backup files to a separate directory
      create-lockfiles nil                                                 ; Disable lockfiles as I use only one Emacs instance
      )

(require 'init-straight)  ;; staight + use-package 管理插件包方案
;; (require 'init-benchmarking) ;; Measure startup time

;; Bootstrap config
(require 'init-copy)
(require 'init-display)
(require 'init-edit)
(require 'init-locale)
(require 'init-dired)

;; packages
(require 'init-org)
;; (require 'init-ivy)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Custom file
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
