 ;https://github.com/weirdNox/org-noter
(use-package org-noter
  :ensure t
;  :after org-mode
                                        ;  :hook (org-mode . org-noter)
  :bind(("C-c n o" . org-noter));make notes with place pdf
  :config
  (setq org-noter-always-create-frame nil)
  )
