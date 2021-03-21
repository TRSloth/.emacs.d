(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(load "~/.emacs.d/my-paths.el")

;;; Use-Package config and auto-package-update

;https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t);install not already installed packages


(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))


;;; Files to keep further installs tidy

;https://github.com/emacscollective/no-littering
(use-package no-littering
  :ensure t
  :init
  (setq no-littering-var-directory (expand-file-name "cache/data/" user-emacs-directory))
  (setq no-littering-etc-directory (expand-file-name "cache/etc/" user-emacs-directory))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :ensure t
  :config
  (setq page-break-lines-mode 1))
