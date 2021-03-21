(use-package pdf-tools
  :ensure t
  :pin melpa
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq pdf-annot-activate-created-annotations t)
  :bind (:map pdf-view-mode-map
              (("C-f" . isearch-forward))
))
