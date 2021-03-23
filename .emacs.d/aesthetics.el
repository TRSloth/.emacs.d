;;; Applying Themes
;; Fonts
;;(set-face-attribute 'default nil :font "DejaVu Sans-12")b
(set-face-attribute 'default nil :font "dubai-12")
;;(set-face-attribute 'default nil  :font "courier-12")
;; Theme
(use-package modus-operandi-theme :ensure t  :defer t)
(use-package modus-vivendi-theme :ensure t :defer t)
;(load-theme 'modus-operandi t)
(load-theme 'modus-vivendi t)
;(dolist (theme custom-enabled-themes)  (disable-theme theme))


;;; built-in look and feel options
(setq-default word-wrap t)
(setq global-visual-line-mode t)
(setq-default line-spacing 0)
(setq x-underline-at-descent-line t)
(setq-default  tooltip-mode nil)
(setq-default  cursor-type 'bar)
(set-cursor-color "#ffffff")
(setq blink-cursor-mode nil)
(setq pop-up-windows nil)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode 1)
(toggle-scroll-bar 1)
(display-time-mode 1)
(display-battery-mode 1)

