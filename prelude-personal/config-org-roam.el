;https://github.com/org-roam/org-roam
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

;https://github.com/org-roam/org-roam-bibtex
(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
:bind (("C-c n c" . orb-insert));make notes on citations
)

;https://github.com/org-roam/org-roam-server
;Install guide                                        ;https://github.com/nobiot/Zero-to-Emacs-and-Org-roam/blob/main/90.org-protocol.md
(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))
