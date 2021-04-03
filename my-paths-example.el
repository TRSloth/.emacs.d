;;;; Customise this for your directory structure
; You may edit this file directly or duplicate and remove "-example" to favor your version
; Suggested changes are commented "MUST" "SHOULD" & "MAY" to help you check their values


;;;; Set user-emacs-directory
; This should point to the directory this file is in, where ~ is the "HOME" variable below
(setq user-emacs-directory "~/dot-emacs/")				; MUST


;;;; Link files in Windows
(when (string-equal system-type "windows-nt")
  (setenv "HOME" "C:/repo/IndividualProject_2020_Toby-Rowlands"))	; MUST, IF using Windows,

;;;; Link files in Linux
(when (string-equal system-type "gnu/linux")
  (setenv "HOME" "/home/chaos")			       			; MUST, change if using LINUX to the parent dir of this directory
  (setq TeX-macro-global "/usr/bin/"))					; MAY, set TeX install location


;;; Directory check
; If given user directory could not be found but ".emacs.d/" could use that
; Else create the user-directory
(setq user-emacs-directory (locate-user-emacs-file user-emacs-directory ".emacs.d/"))


;;;; Finish importing Windows functions now user-emacs-dir has been checked
(when (string-equal system-type "windows-nt")
  (add-to-list 'exec-path (concat user-emacs-directory "win")))	      	; locate required windows files(included)
  (setq pdf-info-epdfinfo-program (concat user-emacs-directory "win/epdfinfo.exe"))
  (setq TeX-macro-global						; MAY add where you've downloaded Texlive, required for Latex
   '("c:/texlive/2020/texmf-var/tex/" "c:/texlive/texmf-local/tex/" "c:/texlive/texmf-local/bibtex/bst/" "c:/texlive/2020/texmf-dist/tex/" "c:/texlive/2020/texmf-dist/bibtex/bst/"))
  (load (concat user-emacs-directory "win/win-keys.el") 'noerror)	; Adds some Windows Keybinds(not many yet) MAY remove this
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


;;;; Program specific links 						; SHOULD, set these to the directorys you are using under HOME
(setq default-directory "~/docs/")					; default when searching
(setq org-directory "~/docs/")						; where your org-notes are stored
(setq org-download-image-dir "~/gdrive/Library/img/misc/")		; Where you want images downloaded with org-download stored
(setq org-roam-directory "~/docs/education-notes")		       	; Where you keep your org-roam notes
(setq org-brain-path "~/docs/sd-plan")					; Structured org-brain notes
(setq org-ref-pdf-directory "~/gdrive/Library/")
(setq org-ref-default-bibliography "~/docs/writeup/roam.bib") 		; Your main bibleography
(setq bibtex-completion-bibliography "~/docs/writeup/roam.bib")


