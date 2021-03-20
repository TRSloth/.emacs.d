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
 '(current-language-environment "UTF-8")
 '(custom-safe-themes t)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(desktop-path '("~/.emacs.d/desktops"))
 '(doc-view-continuous t)
 '(doc-view-scale-internally nil)
 '(electric-indent-mode nil)
 '(global-visual-line-mode t)
 '(image-file-name-extensions
   '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg" "pdf" "bmp"))
 '(inhibit-startup-screen t)
 '(menu-bar-mode t)
 '(org-hide-block-startup nil)
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
 '(package-selected-packages
   '(poly-markdown poly-org polymode org-brain pdf-continuous-scroll-mode quelpa-use-package quelpa helm-org-rifle buffer-move dired-open org-kindle countdown egg-timer org-pomodoro org-roam-server company-bibtex company-auctex company desktop+ graphviz-dot-mode org-noter-pdftools org-noter org-roam-bibtex ivy-bibtex org-ref bibtex-completion latex-preview-pane yasnippet pdf-tools auctex org-superstar org-download counsel swiper ivy modus-vivendi-theme modus-operandi-theme org-roam))
 '(quelpa-upgrade-p t)
 '(safe-local-variable-values '((TeX-master . master))))
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
(setq org-roam-graph-executable "~/.emacs.d/addons-win/Graphviz/bin/dot.exe")
(setq desktop+-base-dir "~/.emacs.d/desktops/" )
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
;-------Started replacing with use package-----
(setq org-directory "~/entropy/")
(use-package swiper
   :ensure t
 :config
 (global-set-key "\C-f" 'swiper))

 (use-package counsel
   :ensure t
   :config
   (global-set-key (kbd "M-x") 'counsel-M-x)
   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
   (global-set-key (kbd "C-h f") 'counsel-describe-function)
   (global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-c M-f") #'counsel-recentf))

(use-package helm-bibtex
  :ensure t
  :init
 (autoload 'helm-bibtex "helm-bibtex" "" t)
  :config
  (require 'helm)
  (setq bibtex-completion-bibliography '("~/entropy/roam.bib"))
 :bind (("C-c h b" . helm-bibtex)
("C-c h B" . helm-bibtex-with-local-bibliography)
("C-c h n" . helm-bibtex-with-notes)
("C-c h h" . helm-resume)))

(use-package org-roam
      :ensure t
      :init
      (setq org-roam-db-update-method 'immediate)
      (setq org-roam-completion-everywhere t)
      (setq org-roam-db-gc-threshold most-positive-fixnum)
      (setq org-roam-graph-viewer nil) 
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/entropy/")
      :bind (:map org-roam-mode-map
              ;(("C-c n l" . org-roam)
              (("C-c n /" . org-roam-find-file)
              ; ("C-c n g" . org-roam-graph)
              ( "C-c n r" . org-roam-buffer-toggle-display))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))
              (("C-c n b" . org-roam-switch-to-buffer))
              (("C-c n d" . org-roam-find-directory))
))

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(add-hook 'after-init-hook 'global-company-mode)
(require 'company-auctex)
(company-auctex-init)
(require 'company-bibtex)
(add-to-list 'company-backends 'company-bibtex)


(use-package pdf-tools
  :ensure t
  :pin melpa
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
 ; (define-key pdf-view (kbd "C-f") 'isearch-forward) ;counsel search not work in pdf view)
 :bind (:map pdf-view-mode-map
              (("C-f" . isearch-forward))
))


;;-----Org-download-----;

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
(setq-default org-download-image-dir "~/gdrive/Library/img/misc/")

;-------End Replacing with use-package
;(add-hook 'after-init-hook 'pdf-tools-install)
;(add-hook 'after-init-hook #'org-roam-bibtex-mode)
(add-hook 'TeX-mode-hook 'TeX-fold-mode)      ; auto-activate TeX-fold-mode
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)  ; auto-activate math mode
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
;(autoload 'helm-bibtex "helm-bibtex" "" t);;no idea where this came from or what it does
(put 'set-goal-column 'disabled nil)
(ivy-mode 1)
(desktop-save-mode -1)
(set-language-environment "UTF-8")
;;-----General options-----;;
;(setq org-roam-db-update-method 'immediate)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq initial-major-mode 'org-mode)
(setq version-control t)
(setq backup-directory-alist  '(("." . "~/.emacs.d/file-backups")))

(require 'org-protocol) ;https://github.com/nobiot/Zero-to-Emacs-and-Org-roam/blob/main/90.org-protocol.md
(require 'org-roam-protocol)
(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))


;;----- Bibtex options ----- ;;
(setq company-bibtex-bibliography '("~/entropy/roam.bib"))
(setq reftex-default-bibliography '("~/entropy/roam.bib"))
(setq org-ref-default-bibliography '("~/entropy/roam.bib"))
;(setq bibtex-completion-bibliography '("~/entropy/roam.bib"))
(setq org-ref-pdf-directory '("~/gdrive/Library/"))
(setq bibtex-completion-library-path '("~/gdrive/Library"))
(setq bibtex-completion-pdf-extension '(".pdf" ".pptx" ".docx"));;file types to recognise
(setq bibtex-completion-pdf-open-function  (lambda (fpath)   (start-process "open" "*open*" "open" fpath)))
(setq bibtex-completion-additional-search-fields '(keywords));Allows for search bib by keyword

;;------Other------;;;
(require 'buffer-move)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;----- Latex options ----- ;;
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq latex-run-command "pdflatex")
(setq safe-local-variable-values '((TeX-master . master)))
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")); "If you plan to build PDF files via LaTeX you need to make sure that org-latex-pdf-process is set to process the bibliography (using bibtex or biblatex). Here is one example of how to do that (see ./org-ref.org::*LaTeX export for other alternatives)."

(add-hook 'LaTeX-mode-hook (lambda ()
                 (push 
                  '("Latex_outdir" "%`pdflatex --output-directory=/tmp %(mode)%' %t" 
                TeX-run-TeX nil (latex-mode doctex-mode) 
                :help "Run pdflatex with output in /tmp")
                  TeX-command-list)))

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
(with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
                '("final_report"
                  "\\documentclass{final_report}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


(setq orb-preformat-keywords
   '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point)
         ""
         :file-name "${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}
#+roam_alias: 
#+roam_tags: 
* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: ${file}:
:END:
- keywords :: ${keywords}
")))


(require 'egg-timer)


(setq quelpa-upgrade-interval 7);auto upgrade quelpa files every days
(add-hook #'after-init-hook #'quelpa-upgrade-all-maybe)
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(setq use-package-always-ensure t)
(require 'quelpa-use-package)

(add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)
(require 'helm-org-rifle)




;;;---org-brain----;

(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/entropy/brain")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

;; Allows you to edit entries directly from org-brain-visualize
(use-package polymode
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode)
(with-eval-after-load "polymode"
  (define-hostmode org-brain-poly-hostmode
    :mode 'org-brain-visualize-mode)))

(load "~/.emacs.d/config/aesthetics.el")
(load "~/.emacs.d/config/commands.el")
(load "~/.emacs.d/config/keybinds.el")
