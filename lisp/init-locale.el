;;; init-locale.el --- config locale -*- lexical-binding: t -*-
;;; Commentary:

;; 区域设置
;; 编码设置

;;; Code:

;; 中文显示
(set-language-environment "utf-8")
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-clipboard-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default pathname-coding-system 'utf-8)
(setq
 default-process-coding-system '(utf-8 . utf-8)
 locale-coding-system 'utf-8
 file-name-coding-system 'utf-8
 default-buffer-file-coding-system 'utf-8
 slime-net-coding-system 'utf-8-unix)

(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")                           ; Iterate through CamelCase words

(provide 'init-locale)
