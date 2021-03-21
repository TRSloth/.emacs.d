;https://github.com/tmalsburg/helm-bibtex
(use-package helm-bibtex
  :ensure t
  :init
  (autoload 'helm-bibtex "helm-bibtex" "" t)
  :config
  (require 'helm)
  (setq bibtex-completion-bibliography '("~/entropy/roam.bib"))
  :bind (("C-c h b" . helm-bibtex)
         ("C-c h B" . helm-bibtex-with-local-bibliography)
         ("C-c h n" . helm-bibtex-with-notes)
         ("C-c h h" . helm-resume))
)
