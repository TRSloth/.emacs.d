;;; ------Themeing----- ;;;
;; Fonts
;;(set-face-attribute 'default nil :font "DejaVu Sans-12")b
(set-face-attribute 'default nil :font "dubai-12")
;;(set-face-attribute 'default nil  :font "courier-12")
;; Theme
;;(load-theme 'modus-operandi)
(load-theme 'modus-vivendi)
;(dolist (theme custom-enabled-themes)  (disable-theme theme))

(setq bibtex-completion-pdf-symbol "⌘") ;appears if pdf exists
(setq bibtex-completion-notes-symbol "✎") ;appears if notes exist
(setq pdf-annot-activate-created-annotations t)

;;----- Asthetic options ----;;
(setq-default word-wrap t)
(setq-default line-spacing 0)
(setq x-underline-at-descent-line t)
(setq-default  tooltip-mode nil)
(tool-bar-mode -1)
(menu-bar-mode 1)
(toggle-scroll-bar 1)
(display-time-mode 1)
(setq-default  cursor-type 'bar)
(set-cursor-color "#ffffff") 
(setq blink-cursor-mode nil)
(setq org-hide-leading-stars t)
(setq org-ellipsis "⤵")
(setq org-image-actual-width '(600))
(setq pop-up-windows nil)
(setq org-noter-always-create-frame nil)
(setq org-superstar-headline-bullets-list '(9673 9675 9671 10047))
(setq org-startup-folded nil)
(setq org-startup-with-inline-images t)
