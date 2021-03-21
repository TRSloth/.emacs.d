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
  :init
  (setq page-break-lines-mode 1))


;;; swiper-helm and counsel for navigation
;;; was swiper and counsel for navigation

(use-package swiper-helm
   :ensure t
 :config
 (global-set-key "\C-f" 'swiper))

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (setq helm-mode 1))

; (use-package counsel
;   :ensure t
;   :config
;   (global-set-key (kbd "M-x") 'counsel-M-x)
;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;   (global-set-key (kbd "C-h f") 'counsel-describe-function);use helm-apropos C-X c a
;   (global-set-key (kbd "C-h v") 'counsel-describe-variable)
;   (global-set-key (kbd "C-c M-f") 'counsel-recentf))


