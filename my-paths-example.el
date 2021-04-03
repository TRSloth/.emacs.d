;;;; Customise this for your directory structure
; You may edit this file directly or duplicate and remove "-example" to favor your version
; Suggested changes are commented "MUST" "SHOULD" & "MAY" to help you check their values


;;;; Set user-emacs-directory
; This should point to the directory this file is in, where ~ is the "HOME" variable below
(setq user-emacs-directory "~/dot-emacs/")				; MUST


;;;; Link files in Windows
(when (string-equal system-type "windows-nt")
  (setenv "HOME" "C:/Repos/IndividualProject_2020_Toby-Rowlands")	; this is where '~' refers to
  (setq user-emacs-directory "~/.emacs.d/")
(setq pdf-info-epdfinfo-program "~/.emacs.d/win/epdfinfo.exe")
; (setq org-roam-graph-executable "~/.emacs.d/win/Graphviz/bin/dot.exe"); allows org-roam-graph, though functionality is eclipsed by org-roam-server-mode so I've removed it
  (setq TeX-macro-global						; where you've downloaded Texlive only required for Latex
   '("c:/texlive/2020/texmf-var/tex/" "c:/texlive/texmf-local/tex/" "c:/texlive/texmf-local/bibtex/bst/" "c:/texlive/2020/texmf-dist/tex/" "c:/texlive/2020/texmf-dist/bibtex/bst/"))
;;; Locate linux functions in windows
  (add-to-list 'exec-path "~/.emacs.d/win") 				; additional Windows files already installed
  (setenv "PATH"
    (concat							       	; Paths for Windows version of linux executables
     "C:/msys64/mingw64/bin" ";"					; if you have it installed it is prefered over cygwin
     "C:/texlive/2020/bin/win32" ";" 					; PDFlatex
     "C:/cygwin64/usr/local/bin" ";"					; Cygwin
     "C:/cygwin64/usr/sbin" ";"
     "C:/cygwin64/bin" ";"
     "C:/Program Files/Git/bin" ";"	 				; Git
     (getenv "PATH")))
  (load "~/.emacs.d/win/win-keys.el" 'noerror))


;;;; Link files in Linux
(when (string-equal system-type "gnu/linux")
  (setenv "HOME" "/home/chaos")			       			; this is where '~' refers to
  (setq user-emacs-directory "~/.emacs.d/")
    (setq TeX-macro-global "/usr/bin/")
   )
  

;;;; Program specific links 						; SHOULD, set these to the directorys you are using under HOME
(setq default-directory "~/docs/")					; default when searching
(setq org-directory "~/docs/")						; where your org-notes are stored
(setq org-download-image-dir "~/gdrive/Library/img/misc/")		; Where you want images downloaded with org-download stored
(setq org-roam-directory "~/docs/education-notes")		       	; Where you keep your org-roam notes
(setq org-brain-path "~/docs/sd-plan")					; Structured org-brain notes
(setq org-ref-pdf-directory "~/gdrive/Library/")
(setq org-ref-default-bibliography "~/docs/writeup/roam.bib") 		; Your main bibleography
(setq bibtex-completion-bibliography "~/docs/writeup/roam.bib")
