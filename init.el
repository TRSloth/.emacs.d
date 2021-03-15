(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; Note comments on custom will be removed
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-macro-global
   '("c:/texlive/2020/texmf-var/tex/" "c:/texlive/texmf-local/tex/" "c:/texlive/texmf-local/bibtex/bst/" "c:/texlive/2020/texmf-dist/tex/" "c:/texlive/2020/texmf-dist/bibtex/bst/"))
 '(TeX-macro-private '("g:/Library/Latex/"))
 '(auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")
 '(auto-save-visited-mode t)
 '(blink-cursor-mode nil)
 '(current-language-environment "UTF-8")
 '(cursor-type 'bar)
 '(custom-safe-themes t)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(doc-view-continuous t)
 '(doc-view-scale-internally nil)
 '(electric-indent-mode nil)
 '(global-visual-line-mode t)
 '(image-file-name-extensions
   '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg" "pdf" "bmp"))
 '(inhibit-startup-screen t)
 '(menu-bar-mode t)
 '(org-ellipsis "$(O#/(B")
 '(org-hide-block-startup nil)
 '(org-hide-leading-stars t)
 '(org-image-actual-width '(600))
 '(org-latex-default-packages-alist
   '(("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t nil)
     ("" "grffile" t nil)
     ("" "longtable" nil nil)
     ("" "wrapfig" t nil)
     ("" "rotating" nil nil)
     ("normalem" "ulem" t nil)
     ("" "amsmath" t nil)
     ("" "textcomp" t nil)
     ("" "amssymb" t nil)
     ("" "capt-of" nil nil)
     ("" "hyperref" t nil)
     ("" "comment" t nil)
     ("" "biblatex" t nil)
     ("" "acronym" t nil)))
 '(org-latex-pdf-process '("latexmk -shell-escape -bibtex -f -pdf %f"))
 '(org-noter-always-create-frame nil)
 '(org-roam-directory "g:/entropy/")
 '(org-startup-folded nil)
 '(org-startup-with-inline-images t)
 '(org-superstar-headline-bullets-list '(9673 9675 9671 10047))
 '(package-selected-packages
   '(org-noter-pdftools org-noter org-roam-bibtex ivy-bibtex org-ref bibtex-completion latex-preview-pane yasnippet pdf-tools auctex org-superstar org-download counsel swiper ivy modus-vivendi-theme modus-operandi-theme org-roam))
 '(pop-up-windows nil)
 '(safe-local-variable-values '((TeX-master . master)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode t)
 '(version-control t)
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;------------------------------ End Of Custom -------------------------------

;------------------------------ Adding paths -------------------------------

;;; ------Adding load path-------
;; Allows librarys to be added by me ;;; not in use
;(add-to-list 'load-path "~/.emacs.d/lisp/")
;(let ((default-directory  "~/.emacs.d/lisp/"))
;  (normal-top-level-add-to-load-path '("."))
;  (normal-top-level-add-subdirs-to-load-path))

(setq backup-directory-alist 
  '(("." . "~/.emacs.d/file-backups")))

;;; ------Link files in Linux -----
(when (string-equal system-type "gnu/linux")
(setq user-emacs-directory "/gdrive/.emacs.d/")
(setq default-directory "/gdrive")
(setenv "HOME" "/gdrive"))

;;; ------Link files in Windows-----
(when (string-equal system-type "windows-nt")
(setq user-emacs-directory "g:/.emacs.d/") ;;needs to be g
(setq default-directory "~")
(setenv "HOME" "G:") ;;needs to be g
;;; ------Find linux functions in windows-----  
  (add-to-list 'exec-path "~/.emacs.d/addons-win") ;; sqllite3.exe for use by org-roam
  (setenv "PATH"
    (concat
     "~/.emacs.d/addons-win/diff-bin" ";" ;;Custom adding of diff to install
     "C:/msys64/mingw64/bin" ";"
     "C:/texlive/2020/bin/win32" ";" ;;PDFlatex
     "C:/cygwin64/usr/local/bin" ";"
     "C:/cygwin64/usr/sbin" ";"
     "C:/cygwin64/bin" ";"
;     "C:/Program Files/Git/bin" ";" ;;All already included
     (getenv "PATH"))))

;---------End Paths --------------------

;---------Activate options-------

(setq TeX-PDF-mode t)                         ; compile with PDFLaTeX by default
(add-hook 'TeX-mode-hook 'TeX-fold-mode)      ; auto-activate TeX-fold-mode
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)  ; auto-activate math mode
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
;; Recommendation for Windows users for performance
;; https://github.com/org-roam/org-roam/issues/1289#issuecomment-744046148
(setq org-roam-db-update-method 'immediate)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(put 'set-goal-column 'disabled nil)
(add-hook 'after-init-hook 'pdf-tools-install)
(setq enable-recursive-minibuffers t)
(setq initial-major-mode 'org-mode)
(setq latex-run-command "pdflatex")
(add-hook 'LaTeX-mode-hook (lambda ()
                 (push 
                  '("Latex_outdir" "%`pdflatex --output-directory=/tmp %(mode)%' %t" 
                TeX-run-TeX nil (latex-mode doctex-mode) 
                :help "Run pdflatex with output in /tmp")
                  TeX-command-list)))

(with-eval-after-load 'ox-latex ; add cls files
   (add-to-list 'org-latex-classes
                '("mitthesis"
                  "\\documentclass{mitthesis}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
(with-eval-after-load 'ox-latex ; add cls files
   (add-to-list 'org-latex-classes
                '("tobysanswers"
                  "\\documentclass{tobysanswers}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


;;; ------Themeing----- ;;;
;; Fonts
;;(set-face-attribute 'default nil :font "DejaVu Sans Mono-12")
(set-face-attribute 'default nil :font "dubai-12")
;;(set-face-attribute 'default nil  :font "courier-12")
;; Automatically load the theme you like
;; I am using modus-operandi (light theme) here
;; There is also modus-vivendi (dark theme)
;;(load-theme 'modus-operandi)
;;----- Asthetic options ----- ;;

(setq-default word-wrap t)
(setq-default line-spacing 0)
(setq x-underline-at-descent-line t)

;-----------------Bibleography funcs ----------------------

;;;; Org ref ;;;; https://github.com/jkitchin/org-ref

(setq reftex-default-bibliography '("~/entropy/roam.bib"))
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")); "If you plan to build PDF files via LaTeX you need to make sure that org-latex-pdf-process is set to process the bibliography (using bibtex or biblatex). Here is one example of how to do that (see ./org-ref.org::*LaTeX export for other alternatives)."

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/entropy/notes/"
      org-ref-default-bibliography '("~/entropy/roam.bib")
      org-ref-pdf-directory '("~/Library/"))
(require 'org-ref)

;;;; Helm-bibtex ;;;; https://github.com/tmalsburg/helm-bibtex

(setq bibtex-completion-bibliography '("~/entropy/roam.bib")
      bibtex-completion-library-path '("~/Library")
      bibtex-completion-notes-path "~/entropy/notes")

(setq bibtex-completion-pdf-field "File");; location of pdf may be specified in a field "File" the even allows for supplementry material to be stored e.g. "File = {:/path/to/article.pdf:PDF;:/path/to/supplementary_materials.pdf:PDF}"
(setq bibtex-completion-pdf-extension '(".pdf" ".pptx" ".docx"));;file types to recognise

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

(setq bibtex-completion-additional-search-fields '(keywords));Allows for search bib by keyword
(setq bibtex-completion-pdf-symbol "$(O'|(B") ;appears if pdf exists
(setq bibtex-completion-notes-symbol "$,2%N(B") ;appears if notes exist


;;;; ORB - org-roam-bibtex setup
(require 'org-roam-bibtex)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)
(autoload 'helm-bibtex "helm-bibtex" "" t);;no idea where this came from or what it does
;;;Paused window 
(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))
(ad-activate 'pop-to-buffer)


 (setq pdf-annot-activate-created-annotations t)

(require 'helm-config)
(load "~/.emacs.d/config/commands.el")
(load "~/.emacs.d/config/keybinds.el")
