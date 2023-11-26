;;; init-display.el --- 外观配置 -*- lexical-binding: t -*
;;; Commentary:

;; config time format, window, font etc.

;;; Code:

;; Font
;; Download Victor Mono at https://rubjo.github.io/victor-mono/
(set-face-attribute 'default nil
                    :family "Victor Mono" :height 145 :weight 'normal)

;; copy from: https://github.com/cabins/emacs.d/blob/1bebdb984dd08ada00b4174b9fb4bd23464985a3/init.el
;; 操作系统变量名称
(defvar cc-os-win (memq system-type '(ms-dos windows-nt cygwin)))
(defvar cc-os-mac (eq system-type 'darwin))

;; 基本设置
(setq-default
 indicate-buffer-boundaries 'left ;; 在窗口边缘上显示一个小箭头指示当前 buffer 的边界
 delete-by-moving-to-trash t                      ;; 删除文件移动到垃圾箱
 window-combination-resize t                      ;; 新窗口平均其他左右窗口
 x-stretch-cursor t                               ;; 将光标拉伸到字形宽度
 kill-whole-line t)  ;; C-k时,同时删除该行

;; 时间显示设置
(display-time-mode 1)   ;; 启用时间显示设置,在minibuffer上面的那个杠上
(setq display-time-24hr-format t   ;; 时间使用24小时制
      display-time-day-and-date t   ;; 时间显示包括日期和具体时间
      display-time-use-mail-icon t   ;; 时间栏旁边启用邮件设置
      display-time-interval 10   ;; 时间的变化频率
      display-time-format "%A %H:%M")   ;; 显示时间的格式

(unless (string-match-p "^Power N/A" (battery))   ; 笔记本上显示电量
  (display-battery-mode 1))

;; visual-line-mode
(add-hook 'after-init-hook 'global-visual-line-mode)

;; fido-mode buffer 垂直显示
;; `fido-mode' is provided by icomplete.el
;; (use-package icomplete
  ;; :hook (after-init . fido-vertical-mode)
  ;; :config (setq completions-detailed t))

;; Highlight Current Line
(use-package hl-line
  :when (display-graphic-p)
  :hook (prog-mode . hl-line-mode))

;;; startup
;; maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen 1)
;;(add-hook 'emacs-startup-hook (lambda () (split-window-right)))

(setq desktop-load-locked-desktop t) ; don't popup dialog ask user, load anyway
(setq desktop-restore-frames nil) ; don't restore any frame
(setq desktop-restore-forces-onscreen nil)
(desktop-save-mode t)

;;(recentf-mode t)
;; (defun open-all-recent-files ()
;;    "Open all recent files."
;;    (interactive)
;;    (dolist (file  recentf-list) (find-file file)))

(provide 'init-display)

;;; init-display.el ends here
