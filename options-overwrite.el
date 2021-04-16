;;; Program specific paths 						; SHOULD, set these to the directorys you are using under HOME(set in .emacs)
; if you use different file structures on each OS you may wish to add OS-specific pathis in the settings below 
(setq
 default-directory "~/docs/"						; default when searching
 org-directory "~/docs/"						; where your org-notes are stored
 org-download-image-dir "~/Library/img/misc/"			; Where you want images downloaded with org-download stored
 org-roam-directory "~/docs"		       				; Where you keep your org-roam notes
 org-brain-path "~/docs/sd-plan"					; Structured org-brain notes
 org-ref-pdf-directory "~/docs/Library/"
 org-ref-default-bibliography "~/docs/writeup/main.bib" 		; Your main bibleography
 reftex-default-bibliography `("~/docs/writeup/main.bib") 
 bibtex-completion-bibliography `("~/docs/writeup/main.bib")
)
