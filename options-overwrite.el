;;; Program specific paths 						; SHOULD, set these to the directorys you are using under HOME(set in .emacs)
; if you use different file structures on each OS you may wish to add OS-specific pathis in the settings below 
(setq
 default-directory "~/digfog/"						; default when searching
 org-directory "~/digfog/"						; where your org-notes are stored
 org-download-image-dir "../gdrive/Library/img/misc/"			; Where you want images downloaded with org-download stored
 org-roam-directory "~/digfog/roam"		       				; Where you keep your org-roam notes
 org-brain-path "~/digfog/brain"					; Structured org-brain notes
 org-ref-pdf-directory "../gdrive/Library/"
 org-ref-default-bibliography "~/digfog/main.bib" 		; Your main bibleography
 reftex-default-bibliography `("~/digfog/main.bib") 
 bibtex-completion-bibliography `("~/digfog/main.bib")
 bibtex-completion-library-path "../gdrive/Library/"
)
