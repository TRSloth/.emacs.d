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
 '(TeX-macro-private '("~/gdrive/Library/Latex/"))
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
 '(org-roam-directory "~/entropy/")
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


;;; ------Link files in Linux -----
(when (string-equal system-type "gnu/linux")
(setq user-emacs-directory "/home/chaos/.emacs.d/")
(setq default-directory "~/gdrive")
(setenv "HOME" "/home/chaos"))

;;; ------Link files in Windows-----
(when (string-equal system-type "windows-nt")
(setq user-emacs-directory "g:/.emacs.d/") ;;needs to be g
(setq default-directory "~/gdrive")
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

(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'after-init-hook 'pdf-tools-install)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)
(add-hook 'TeX-mode-hook 'TeX-fold-mode)      ; auto-activate TeX-fold-mode
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)  ; auto-activate math mode
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(autoload 'helm-bibtex "helm-bibtex" "" t);;no idea where this came from or what it does
(put 'set-goal-column 'disabled nil)
(ivy-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;-----General options-----;;
(setq org-roam-db-update-method 'immediate)
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq initial-major-mode 'org-mode)
(setq latex-run-command "pdflatex")
(setq pop-up-windows nil)
(setq safe-local-variable-values '((TeX-master . master)))
(setq scroll-bar-mode nil)
(setq tool-bar-mode nil)
(setq tooltip-mode nil)
(setq version-control t)

;;----- Asthetic options ----;;
(setq-default word-wrap t);;;;;;;;;;;;;;;;;;;;;;;
(setq-default line-spacing 0);;;;;;;;;;;;;
(setq x-underline-at-descent-line t);;;;;;;;;;;;;;
(setq backup-directory-alist "~/.emacs.d/file-backups")

;;----- Bibtex options ----- ;;

(setq reftex-default-bibliography '("~/entropy/roam.bib"))
(setq org-ref-default-bibliography '("~/entropy/roam.bib"))
(setq bibtex-completion-bibliography '("~/entropy/roam.bib"))
(setq org-ref-pdf-directory '("~/gdrive/Library/"))
(setq bibtex-completion-library-path '("~/gdrive/Library"))
(setq bibtex-completion-pdf-extension '(".pdf" ".pptx" ".docx"));;file types to recognise
(setq bibtex-completion-pdf-open-function  (lambda (fpath)   (start-process "open" "*open*" "open" fpath)))
(setq bibtex-completion-additional-search-fields '(keywords));Allows for search bib by keyword
(setq bibtex-completion-pdf-symbol "$(O'|(B") ;appears if pdf exists
(setq bibtex-completion-notes-symbol "$,2%N(B") ;appears if notes exist
(setq pdf-annot-activate-created-annotations t)
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")); "If you plan to build PDF files via LaTeX you need to make sure that org-latex-pdf-process is set to process the bibliography (using bibtex or biblatex). Here is one example of how to do that (see ./org-ref.org::*LaTeX export for other alternatives)."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook (lambda ()
                 (push 
                  '("Latex_outdir" "%`pdflatex --output-directory=/tmp %(mode)%' %t" 
                TeX-run-TeX nil (latex-mode doctex-mode) 
                :help "Run pdflatex with output in /tmp")
                  TeX-command-list)))

;;; ------Themeing----- ;;;
;; Fonts
(set-face-attribute 'default nil :font "DejaVu Sans-12")
;;(set-face-attribute 'default nil :font "dubai-12")
;;(set-face-attribute 'default nil  :font "courier-12")
;; Theme
;;(load-theme 'modus-operandi)
;;(load-theme 'modus-vivendi)

;;;--------Custom Latex Classes------;;;

(with-eval-after-load 'ox-latex ; add cls files
   (add-to-list 'org-latex-classes
                '("mitthesis"
                  "\\documentclass{mitthesis}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
(with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
                '("tobysanswers"
                  "\\documentclass{tobysanswers}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(require 'org-ref)
(require 'org-roam-bibtex)
(require 'helm-config)

(load "~/.emacs.d/config/commands.el")
(load "~/.emacs.d/config/keybinds.el")
