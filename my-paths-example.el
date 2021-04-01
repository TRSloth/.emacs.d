;;;; Customise this for your directory structure
; Either change to your variables and remove "-example" from the filename or create a symlink with 
; Windows
; mklink my-paths.el my-paths-example.el
; Linux
; ln -s my-paths-example.el my-paths.el 
; doing it this way will allow you to compare your version to changes I make using diff on Github desktop

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
  


;;;Program specific links 
(setq default-directory "~/gdrive")					; default when searching
(setq org-directory "~/entropy/")					; where your org-notes are stored
(setq org-download-image-dir "~/gdrive/Library/img/misc/")		; Where you want images downloaded with org-download stored
(setq org-roam-directory "~/entropy/")	       				; Where you keep your org-roam notes
(setq org-brain-path "~/entropy/brain")					; Structured org-brain notes
(setq bibtex-completion-bibliography '("~/entropy/roam.bib"))		; Your main bibleography
(setq org-ref-pdf-directory "~/gdrive/Library/")
 (setq org-ref-default-bibliography "~/entropy/roam.bib") 
