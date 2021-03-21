(use-package modus-vivendi-theme 
  :ensure t
  :load-path "themes"
  )

(use-package modus-operandi-theme 
  :ensure t
  :load-path "themes"
  )


;;; Applying Themes
;; Fonts
;;(set-face-attribute 'default nil :font "DejaVu Sans-12")b
(set-face-attribute 'default nil :font "dubai-12")
;;(set-face-attribute 'default nil  :font "courier-12")
;; Theme
;;(load-theme 'modus-operandi)
(load-theme 'modus-vivendi)
;(dolist (theme custom-enabled-themes)  (disable-theme theme))


;;; built-in look and feel options
(setq-default word-wrap t)
(setq-default line-spacing 0)
(setq x-underline-at-descent-line t)
(setq-default  tooltip-mode nil)
(setq-default  cursor-type 'bar)
(set-cursor-color "#ffffff")
(setq blink-cursor-mode nil)
(setq pop-up-windows nil)
(tool-bar-mode -1)
(menu-bar-mode 1)
(toggle-scroll-bar 1)
(display-time-mode 1)
(display-battery-mode 1)
