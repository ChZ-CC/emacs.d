;;; init-org-roam.el --- Roam note -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package org-roam
  :after org
  ;;:straight t
  :config
  (org-roam-setup)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)
  ;; Dailies
  ("C-c n j" . org-roam-dailies-capture-today)
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory (string-join (cons my/dump-brain-root '("content-org")) "/"))
  (org-roam-capture-templates `(("d" "default" plain "%?"
                                 :unnarrowed t
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+TITLE: ${title}
#+AUTHOR: Gray King
#+DATE: %U
#+HUGO_BASE_DIR: ../
#+HUGO_SECTION: notes
")))))

(use-package org-superstar
  ;;:straight t
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

(provide 'init-org-roam)
