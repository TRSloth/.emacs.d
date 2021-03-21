(use-package org-roam
  :ensure t
  :init
  (setq org-roam-db-update-method 'immediate)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-graph-viewer nil)
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/entropy/")
  :bind (:map org-roam-mode-map
              (("C-c n /" . org-roam-find-file)
               ( "C-c n r" . org-roam-buffer-toggle-display))
              :map org-mode-map
              ("C-c n i" . org-roam-insert)
              ("C-c n I" . org-roam-insert-immediate)
              ("C-c n b" . org-roam-switch-to-buffer)
              ("C-c n d" . org-roam-find-directory)
              )
  :config
  (setq org-roam-db-update-method 'immediate)
 )
