;;; init-font.el --- 字体配置 -*- lexical-binding: t -*-
;;; Commentary:

;; 字体配置

;;; Code:

;; 定义字体变量
(defvar cc-fonts-default '("Sometype Mono" "Cascadia Code" "Menlo" "Consolas"))
(defvar cc-fonts-unicode '("Segoe UI Symbol" "Symbola" "Symbol"))
(defvar cc-fonts-emoji '("Noto Color Emoji" "Apple Color Emoji"))
(defvar cc-fonts-cjk '("KaiTi" "STKaiTi" "WenQuanYi Micro Hei"))

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
      (set-face-attribute 'default nil :family f-def  :height 145 :weight 'normal)
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

(provide 'init-font)
;;; init-font.el ends here
