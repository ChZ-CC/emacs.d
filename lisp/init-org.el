;; init-org.el --- org mode -*- lexical-binding: t -*-
;;; Commentary:

;; org mode

;;; Code:

(setq my-org-note-dir (file-truename "~/notes/content-org"))

(use-package org
  :straight (:type git :host github :repo "bzg/org-mode")
  :bind
  ("C-c c" . org-capture)
  ("C-c a o" . org-agenda)
  ("C-c C-." . org-mark-ring-goto)
  :custom
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-odd-level-only nil)
  (org-insert-heading-respect-content nil)
  (org-M-RET-may-split-line '((item) (default . t)))
  (org-special-ctrl-a/e t)
  (org-return-follows-link nil)
  (org-use-speed-commands t)
  (org-startup-align-all-tables nil)
  (org-log-into-drawer nil)
  (org-tags-column 1)
  (org-ellipsis " \u25bc" )
  (org-speed-commands-user nil)
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-completion-use-ido t)
  (org-indent-mode t)
  (org-startup-truncated nil)
  :custom-face
  (org-headline-done ((nil (:strike-through t))))
  :init
  (require 'org-id)
  (defun my/org-id-update-id-locations-current-dir()
    "Update id locations from current dir."
    (interactive)
    (org-id-update-id-locations (directory-files "." t "\.org\$" t)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t))))

(use-package org-roam
  :after org
  :straight t
  :config
  (org-roam-setup)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)
  ;; Dailies
  ("C-c n j" . org-roam-dailies-capture-today)
  :custom
  (find-file-visit-truename t)
  (org-roam-v2-ack t)
  (make-directory my-org-note-dir)
  (org-roam-directory (file-truename my-org-note-dir))
  (org-roam-capture-templates `(("d" "default" plain "%?"
                                 :unnarrowed t
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+TITLE: ${title}
#+AUTHOR: CC
#+DATE: %U
#+HUGO_BASE_DIR: ../
#+HUGO_SECTION: notes

#+HUGO_TAGS: 
#+HUGO_CATEGORIES:
#+HUGO_CUSTOM_FRONT_MATTER: :toc true

#+HUGO_DRAFT: true
#+HUGO_SLUG: 
")))))

(use-package org-superstar
  :straight t
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

(use-package ox-hugo
  :straight t
  :after (ox org-mode))

;; 中文断行折行
(add-hook 'org-mode-hook #'toggle-word-wrap)
;; 同一个窗口打开链接
(setq org-link-frame-setup '((file . find-file)))
;; shift select
(setq org-support-shift-select t)


;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))


;;; org todo
(setq org-log-done t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))


(provide 'init-org)

;;; init-org.el ends here
