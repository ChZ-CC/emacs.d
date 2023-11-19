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
;; 字体配置
(defvar cc-fonts-default '("Sometype Mono" "Cascadia Code PL" "Menlo" "Consolas"))
(defvar cc-fonts-unicode '("Segoe UI Symbol" "Symbola" "Symbol"))
(defvar cc-fonts-emoji '("Noto Color Emoji" "Apple Color Emoji"))
(defvar cc-fonts-cjk '("KaiTi" "STKaiTi" "WenQuanYi Micro Hei"))

(require 'subr-x) ;; cl-loop来自这里

(defun cc-find-font (custom-fonts default-fonts)
  "Get the first installed font from CUSTOM-FONTS and DEFAULT-FONTS."

  (catch 'font
    (dolist (f (append custom-fonts default-fonts))
      (when (find-font (font-spec :family f))
	(throw 'font f)))))

(defun cc-font-setup (&rest args)
  "Setup fonts from ARGS, The accepted args are :default :unicode :emoji :cjk."

  (interactive)
  (when (display-graphic-p)
    (let ((f-def (cc-find-font (plist-get args :default) cc-fonts-default))
	  (f-uni (cc-find-font (plist-get args :unicode) cc-fonts-unicode))
	  (f-emo (cc-find-font (plist-get args :emoji) cc-fonts-emoji))
	  (f-cjk (cc-find-font (plist-get args :cjk) cc-fonts-cjk)))
      (set-face-attribute 'default nil :family f-def)
      (setq face-font-rescale-alist `((,f-cjk . 1.2)))
      (dolist (pair `((unicode  . ,f-uni)
		      (emoji    . ,f-emo)
		      (kana     . ,f-cjk)
		      (han      . ,f-cjk)
		      (bopomofo . ,f-cjk)
		      (cjk-misc . ,f-cjk)))
	(set-fontset-font t (car pair) (font-spec :family (cdr pair)) nil 'prepend)))))

(add-hook 'after-init-hook #'cc-font-setup)
(when (daemonp)
  (add-hook 'after-make-frame-functions
	    (lambda (frame)
	      (with-selected-frame frame (cc-font-setup)))))
;; copy ends

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
