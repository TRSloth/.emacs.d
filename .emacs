;;;; Customise this file to match your directory structure
; Place this file in the location emacs expects the system init file to be or symlink to it.
; Personally I use symlinks to keep everything in the same file.
;
;;; Windows 
;; Expected location(Vista and up):
; C:/Users/<Username>/AppData/Roaming/
;; Symlink:
; mklink C:\Users\<username>\AppData\Roaming\.emacs C:\<path-to-repo>\IndividualProject_2020_Toby-Rowlands\dot-emacs\.emacs
;
;;; Linux
;; Expected location:
; 
;; Symlink:
; ls -s /path/to/this/repo/.emacs ~/.emacs
;
;;; Guidance
; Suggested changes are commented "MUST" "SHOULD" & "MAY" to help you check their values


;;;; Set user-emacs-directory
; This should point to the directory this file is in, where ~ is the "HOME" variable below
(setq user-emacs-directory "~/dot-emacs/")				; SHOULD, if you renamed the repo, set new name here


;;;; Set Home in Windows and non-relative paths
(when (string-equal system-type "windows-nt")
  (setenv "HOME" "C:/repo/IndividualProject_2020_Toby-Rowlands")	; MUST, IF using Windows, set as path to this folder
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))	; MAY, if using markdown set the command line arg used
  (setq TeX-macro-global						; MAY add where you've downloaded Texlive, required for Latex
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

;;;; Set Home in Linux and non-relative paths
(when (string-equal system-type "gnu/linux")
  (setenv "HOME" "/home/chaos")			       			; MUST, change if using LINUX to the parent dir of this directory
  (setq TeX-macro-global "/usr/bin/")					; MAY, set TeX install location
)


;;; Directory check
; If given user directory could not be found but ".emacs.d/" could use that
; Else create the user-directory					; TODO add more options to check
(setq user-emacs-directory (locate-user-emacs-file user-emacs-directory ".emacs.d/"))


;;;; Program specific links 						; SHOULD, set these to the directorys you are using under HOME
; if you use different file structures on each OS you may wish to add OS-specific pathis in the settings below 
(setq
 default-directory "~/docs/"					; default when searching
 org-directory "~/docs/"						; where your org-notes are stored
 org-download-image-dir "~/gdrive/Library/img/misc/"		; Where you want images downloaded with org-download stored
 org-roam-directory "~/docs/education-notes"		       	; Where you keep your org-roam notes
 org-brain-path "~/docs/sd-plan"					; Structured org-brain notes
 org-ref-pdf-directory "~/gdrive/Library/"
 org-ref-default-bibliography "~/docs/writeup/roam.bib" 		; Your main bibleography
 bibtex-completion-bibliography "~/docs/writeup/roam.bib"
)


;;;; Import Windows functions with relative file names now user-emacs-dir has been checked
(when (string-equal system-type "windows-nt")
  (add-to-list 'exec-path (concat user-emacs-directory "win"))	      	; locate required windows files(included)
  (setq pdf-info-epdfinfo-program (concat user-emacs-directory "win/epdfinfo.exe"))
  (load (concat user-emacs-directory "win/win-keys.el") 'noerror)	; MAY, remove this, adds some Windows Keybinds to unbound keys(TODO)
)


;;;; Import Windows functions with relative file names now user-emacs-dir has been checked
(when (string-equal system-type "windows-nt")
  (add-to-list 'exec-path (concat user-emacs-directory "win"))	      	; locate required windows files(included)
  (setq pdf-info-epdfinfo-program (concat user-emacs-directory "win/epdfinfo.exe"))
  (load (concat user-emacs-directory "win/win-keys.el") 'noerror)	; MAY, remove this, adds some Windows Keybinds to unbound keys(TODO)
)


;;;; Start the user init file (init.el)
(setq user-init-file (concat user-emacs-directory "init.el"))		
; Set so emacs doesn't reload this file(.emacs) if you went for the symlink approach
(load user-init-file)
