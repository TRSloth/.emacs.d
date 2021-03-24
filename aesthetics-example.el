;;;; Customise this to your liking
;;; Trying things out
; To test a font or theme scroll to after its final bracket and enter "C-x C-e"

;;; Linking to this file
; Either change to your variables and remove "-example" from the filename or create a symlink with 
; Windows
; mklink aesthetics.el aesthetics-example.el
; Linux
; ln -s aesthetics-example.el aesthetics.el 
; doing it this way will allow you to compare your version to changes I make using diff on Github desktop

;;;; Fonts and Themes
;;; Fonts-unset as I don't know what you've got installed
;(set-face-attribute 'default nil :font "DejaVu Sans-12")		; Monospaced font, more readable than courier		
;(set-face-attribute 'default nil :font "dubai-12")			; My default from https://dubaifont.com/
;(set-face-attribute 'default nil  :font "courier-12")			; Monospaced font usuall Pre-installed
;;; Theme installation
(use-package modus-operandi-theme :ensure t  :defer t)
(use-package modus-vivendi-theme :ensure t :defer t)
;(load-theme 'modus-operandi t)						; light theme
(load-theme 'modus-vivendi t)						; dark theme
;(dolist (theme custom-enabled-themes)  (disable-theme theme))		; disable current theme


;;;; built-in look and feel options
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

