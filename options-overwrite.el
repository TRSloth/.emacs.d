;;; Program specific paths 						; SHOULD, set these to the directorys you are using under HOME(set in .emacs)
; if you use different file structures on each OS you may wish to add OS-specific pathis in the settings below 

(let
  ((working-dir "~/digfog/")
  (bib-file "main.bib")
  (library-dir "~/../gdrive/Library/"))
  (setq
    default-directory (format "%s" working-dir)						; default when searching
    org-directory  (format "%s" working-dir)				; where your org-notes are stored
    org-download-image-dir (format "%simg/misc/" library-dir)		; Where you want images downloaded with org-download stored
    org-roam-directory (format "%sroam/" working-dir)		       				; Where you keep your org-roam notes
    org-brain-path (format "%sbrain" working-dir)					; Structured org-brain notes
    reftex-default-bibliography `(,(format "%s%s" working-dir bib-file))
    ;backquote (`) is a special form of ' that allows evaluating expressions prefixed with (,)
    org-ref-bibliography-notes (format "%sroam/notes.org" working-dir)
    org-ref-default-bibliography `(,(format "%s%s" working-dir bib-file))
    org-ref-pdf-directory (format "%s" library-dir)
    bibtex-completion-bibliography (format "%s%s" working-dir bib-file)
    bibtex-completion-library-path (format "%s" library-dir)
    bibtex-completion-notes-path (format "%snotes" org-roam-directory)
  )
)
