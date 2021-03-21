;https://github.com/jkitchin/org-ref
(use-package org-ref
  :ensure t
  :init
  (setq org-ref-pdf-directory "~/gdrive/Library/")
  (setq org-ref-default-bibliography "~/entropy/roam.bib")
  :bind (("C-c c" . org-ref-helm-insert-cite-link))
)
