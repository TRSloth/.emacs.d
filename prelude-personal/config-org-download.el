(use-package org-download
  :ensure t
  :hook (dired-mode-hook . org-download-enable)
  :config
  (setq-default org-download-image-dir "~/gdrive/Library/img/misc/")
)
