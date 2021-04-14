;;;; Usage

;;; How to use
; Either change to your variables here or create a copy of the file and rename to my-options to override it 
; doing it this way will allow you to compare your version to changes I make using diff on Github desktop

;;; Things you should change
; Everything in the paths section should be configured for your install all else may be left as is



;;;; Paths

;;; Program specific paths 						; SHOULD, set these to the directorys you are using under HOME(set in .emacs)
; if you use different file structures on each OS you may wish to add OS-specific pathis in the settings below 
(setq
 default-directory "~/entropy/"						; default when searching
 org-directory "~/entropy/"						; where your org-notes are stored
 org-download-image-dir "~/gdrive/Library/img/misc/"			; Where you want images downloaded with org-download stored
 org-roam-directory "~/entropy"		       				; Where you keep your org-roam notes
 org-brain-path "~/entropy/brain"					; Structured org-brain notes
 org-ref-pdf-directory "~/gdrive/Library/"
 org-ref-default-bibliography "~/entropy/main.bib" 			; Your main bibleography
 reftex-default-bibliography '("~/entropy/main.bib") 
 bibtex-completion-bibliography '("~/entropy/main.bib")
)


;;; System paths 
; set After program paths so you may override for a specific OS

;; Windows
(when (string-equal system-type "windows-nt")
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))	; MAY, if using markdown set the command line arg use
  (add-to-list 'exec-path "C:/msys64/usr/bin/aspell.exe")
  (setq ispell-program-name "C:/msys64/usr/bin/aspell.exe")
  (setq TeX-macro-global						; MAY add where you've downloaded a TeX editor, required for Latex
   '("c:/texlive/2020/texmf-var/tex/" "c:/texlive/texmf-local/tex/" "c:/texlive/texmf-local/bibtex/bst/" "c:/texlive/2020/texmf-dist/tex/" "c:/texlive/2020/texmf-dist/bibtex/bst/"))
;;; Locate linux functions in windows 
  (setenv "PATH"							; MAY, fill these with your own versions
    (concat							       	; Paths for Windows version of linux executables
     "C:/msys64/mingw64/bin" ";"					; if you have it installed it is prefered over cygwin
     "C:/texlive/2020/bin/win32" ";" 					; PDFlatex
     "C:/cygwin64/usr/local/bin" ";"					; Cygwin
     "C:/cygwin64/usr/sbin" ";"
     "C:/cygwin64/bin" ";"
     "C:/Program Files/Git/bin" ";"	 				; Git
     (getenv "PATH")))
)


;; Linux
(when (string-equal system-type "gnu/linux")
  (setq TeX-macro-global "/usr/bin/")					; MAY, set TeX install location
)


;;;; End Paths


;;;; Aesthetic options

;;; Fonts and Themes
; To test a font or theme scroll to after its final bracket and enter "C-x C-e"
;;; Theme installation
(use-package modus-operandi-theme :defer t)
(use-package modus-vivendi-theme :defer t)
(use-package melancholy-theme :defer t)
(use-package zenburn-theme :defer t)
;(load-theme 'modus-operandi t)						; light theme
;(load-theme 'modus-vivendi t)						; dark themes
;(load-theme 'melancholy t)
(load-theme 'zenburn t)
;(dolist (theme custom-enabled-themes)  (disable-theme theme))		; disable current theme
;
;; Fonts-unset as I don't know what you've got installed
;(set-face-attribute 'default nil :font "Lexend Exa-10")    ; Open Font with scientifically improved reading comprehension
                                                            ; Learn more  https://github.com/ThomasJockin/lexend
;(set-face-attribute 'default nil :font "DejaVu Sans-12")		; Monospaced font, more readable than courier		
;(set-face-attribute 'default nil :font "dubai-10")			    ; My default from https://dubaifont.com/
;(set-face-attribute 'default nil  :font "courier-12")			; Monospaced font usually Pre-installed


;;; built-in look and feel options
(setq-default word-wrap t)
(setq-default line-spacing 0)
(setq x-underline-at-descent-line t)
(setq-default  tooltip-mode nil)
(setq-default  cursor-type 'bar)
(set-cursor-color "#ffffff")
(setq blink-cursor-mode nil)
(setq pop-up-windows nil)
(setq inhibit-startup-screen t)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(display-time-mode 1)
(display-battery-mode 1)
(setq persistent-scratch-file t)
;;; disable error sounds
;(setq ring-bell-function 'ignore)
;(setq visible-bell t)

;;;; End Aesthetics


;;;; Custom keybindings

;;; Pastebin
; This is where I place strings I frequenctly want to paste in
; You can assign any key you like I use keys starting with "C-c a" as only "C-c a c" is already assigned
; C-c a c already defined as inserting acronym
(global-set-key (kbd "C-c a a") "#+roam_alias: ")
(global-set-key (kbd "C-c a s") "#+startup: content\n")
(global-set-key (kbd "C-c a t") "#+roam_tags: ")


;;; Alternatives for Windows keys
(when (string-equal system-type "windows-nt")
  (global-unset-key (kbd "C-\\")) ;unset an easy key to use with windows ctrl methods
  (global-set-key (kbd "C-\\ \\") 'toggle-input-method) ;give key a similar bind in case its still needed
  (global-set-key (kbd "C-\\ w") 'kill-this-buffer) ; "C-w" is copy, so we've added shift
  (global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal) ; usually tied to C-x C-c
; Copy paste versions
  (global-set-key (kbd "C-\\ v") 'clipboard-yank)
  (global-set-key (kbd "C-\\ c") 'kill-ring-save)
  (global-set-key (kbd "C-\\ x") 'kill-region)
)

;;;General Global keymaps

; Copy paste remaps
(global-set-key "\C-v" 'clipboard-yank)
(global-set-key "\M-v" 'yank-pop)
(global-set-key "\C-w" 'kill-ring-save)
(global-set-key "\M-w" 'kill-region)

; Other general maps
(global-set-key (kbd "C-z") 'undo) ;Emacs default is bound to hide Emacs.
(global-set-key (kbd "C-c n t") #'delete-file)
(global-set-key (kbd "C-G") #'abort-recursive-edit)

; change window size
(global-set-key (kbd "M-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-<down>") 'shrink-window)
(global-set-key (kbd "M-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-C C-s") 'isearch-forward)


;;; Add latex class files
(with-eval-after-load 'ox-latex ; add cls files
   (add-to-list 'org-latex-classes
                '("mitthesis"
                  "\\documentclass{mitthesis}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
   (add-to-list 'org-latex-classes
                '("tobysanswers"
                  "\\documentclass{tobysanswers}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
   (add-to-list 'org-latex-classes
                '("final_report"
                  "\\documentclass{final_report}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;;; Local options
(set-language-environment "UTF-8")
(setq ispell-local-dictionary "english")
