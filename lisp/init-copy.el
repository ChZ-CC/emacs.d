;;; init-copy.el --- copypaste from system clipboard -*- lexical-binding: t -*-
;;; Commentary:
;; 系统共享剪切板
;; see also:
;;   https://www.emacswiki.org/emacs/CopyAndPaste
;;   https://www.reddit.com/r/emacs/comments/5n9t3f/copypaste_from_system_clipboard_on_windows/
;;; Code:

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(cond
 ((memq window-system '(x))
  (setq x-select-enable-primary t
        x-select-enable-clipboard nil))
 ((memq window-system '(mac ns))
  (setq interprogram-cut-function 'paste-to-osx
        interprogram-paste-function 'copy-from-osx))
 ((memq window-system '(win32 pc))
  (setq select-enable-primary t
        select-enable-clipboard t
        save-interprogram-paste-before-kill t)))

(provide 'init-copy)

;; init-copy.el ends here
