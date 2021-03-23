(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;;; use-package install
;https://github.com/jwiegley/use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t);always install not already installed packages

;;;load other lisp
(load "~/.emacs.d/my-paths.el")
(load "~/.emacs.d/aesthetics.el")
(load "~/.emacs.d/pastebin.el" 'noerror);Strings I use frequently usually assigned to "C-c a ~" 


;;;auto-package-update
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))


;;;Startup options
(setq debug-on-error t)
(set-language-environment "UTF-8")
(put 'set-goal-column 'disabled nil)
(setq version-control t)
(setq backup-directory-alist  '(("." . "~/.emacs.d/file-backups")))
(setq initial-major-mode 'org-mode)
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")
(setq auto-save-visited-mode t)
(setq delete-old-versions t)
(setq delete-selection-mode t)
(setq custom-safe-themes t)
(setq doc-view-continuous t)
(setq doc-view-scale-internally nil)
(setq electric-indent-mode nil)
(setq image-file-name-extensions
   '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg" "pdf" "bmp"))


;;;General Global keymaps

; Copy paste remaps
(global-set-key "\C-v" 'clipboard-yank)
(global-set-key "\M-v" 'yank-pop)
(global-set-key "\C-w" 'kill-ring-save)
(global-set-key "\M-w" 'kill-region)
(global-set-key "\C-c k" #'cua-mode)

; Other general maps
(global-set-key (kbd "C-z") 'undo) ;Emacs default is bound to hide Emacs.
(global-set-key (kbd "C-c n t") #'delete-file)

; change window size
(global-set-key (kbd "M-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-<down>") 'shrink-window)
(global-set-key (kbd "M-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-C C-s") 'isearch-forward)


;;; Files to keep further installs tidy

;https://github.com/emacscollective/no-littering
(use-package no-littering
  :ensure t
  :init
  (setq no-littering-var-directory (expand-file-name "cache/data/" user-emacs-directory))
  (setq no-littering-etc-directory (expand-file-name "cache/etc/" user-emacs-directory))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :ensure t
  :init
  (setq page-break-lines-mode 1))


;;; navigation -currently using ivy

;https://github.com/abo-abo/swiper
(use-package ivy
  :ensure t
  :config
  (ivy-mode)
 (setq ivy-use-virtual-buffers t)
 (setq enable-recursive-minibuffers t)
 (setq ivy-use-selectable-prompt t)
 (global-set-key (kbd "C-c C-r") 'ivy-resume)
)
  
;https://github.com/abo-abo/swiper
(use-package swiper;-helm ;uncomment helm for helm version
 :ensure t
 :config
 (global-set-key "\C-f" 'swiper))

;;;helm-alternative
;Advantages: is a buffer(appears where you call it), shows recently used
;Disadvantages bloated, couldn't figure roam autocomplete
;; (use-package helm 
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-x") #'helm-M-x)
;;   (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;;   (global-set-key (kbd "C-x C-f") #'helm-find-files)
;;   )

(use-package counsel
   :ensure t
   :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "C-h f") 'counsel-describe-function)
    (global-set-key (kbd "C-x c a") 'counsel-apropos)
   (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-c M-f") 'counsel-recentf)
)

;;; improvements to isearch
;;; used when swiper can't be (inside pdf's etc)
;Wrap search if command not found

;Source: https://stackoverflow.com/a/36707038
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward); remap


;;; org-config
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  :config(progn
  (setq org-directory "~/entropy/")
  (setq org-hide-leading-stars t)
  (setq org-ellipsis "⤵")
  (setq org-image-actual-width '(600))
  (setq org-startup-folded nil)
  (setq org-hide-block-startup nil)
  (setq org-startup-with-inline-images t))
  )

(use-package org-download
  :ensure t
  :hook (dired-mode-hook . org-download-enable)
  :config
  (setq-default org-download-image-dir "~/gdrive/Library/img/misc/")
)

;https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :ensure t
  :after org
  :init
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-headline-bullets-list '(9673 9675 9671 10047))
  )

 ;https://github.com/weirdNox/org-noter
(use-package org-noter
  :ensure t
  :after org
  :hook (org-mode-hook . org-noter)
  :bind(("C-c n o" . org-noter));make notes with place pdf
  :config
  (setq org-noter-always-create-frame nil)
    (require 'org-noter-pdftools))

;https://github.com/alphapapa/org-rifle
(use-package helm-org-rifle
  :ensure t
  :after org)

;;; Org-roam config

;https://github.com/org-roam/org-roam
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
              (("C-c n /" . org-roam-find-file)
               ( "C-c n r" . org-roam-buffer-toggle-display))
              :map org-mode-map
              ("C-c n i" . org-roam-insert)
              ("C-c n I" . org-roam-insert-immediate)
              ("C-c n b" . org-roam-switch-to-buffer)
              ("C-c n d" . org-roam-find-directory)
              ("C-c a c" . fa/add-latex-acronym)
              )
  )

;https://github.com/org-roam/org-roam-server
;Install guide
;https://github.com/nobiot/Zero-to-Emacs-and-Org-roam/blob/main/90.org-protocol.md
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


;;; Bibtex config

;https://github.com/org-roam/org-roam-bibtex
(use-package org-roam-bibtex
  :ensure t
  :after org
  :hook (after-init . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
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
:bind (("C-c n c" . orb-insert));make notes on citations
)

;https://github.com/jkitchin/org-ref
(use-package org-ref
  :ensure t
   :hook (org-mode-hook . org-ref)
  :init
  (setq org-ref-pdf-directory "~/gdrive/Library/")
  (setq org-ref-default-bibliography "~/entropy/roam.bib")
  :bind (("C-c c" . org-ref-helm-insert-cite-link))
  )

;https://github.com/tmalsburg/helm-bibtex
(use-package helm-bibtex
  :ensure t
  :init
  (autoload 'helm-bibtex "helm-bibtex" "" t)
  :config
  (require 'helm)
  (setq bibtex-completion-bibliography '("~/entropy/roam.bib"))
  (setq bibtex-completion-pdf-symbol "⌘") ;appears if pdf exists
 (setq bibtex-completion-notes-symbol "✎") ;appears if notes exist
  :bind (("C-c h b" . helm-bibtex)
         ("C-c h B" . helm-bibtex-with-local-bibliography)
         ("C-c h n" . helm-bibtex-with-notes)
         ("C-c h h" . helm-resume))
)


;;; pdf tools
(use-package pdf-tools
  :ensure t
  :pin melpa
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq pdf-annot-activate-created-annotations t)
  (load "~/.emacs.d/pdf-continuos-scroll-mode.el")
(add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode);this can be installed and auto-updated with quelpa but that took too long to configure last time.
  :bind (:map pdf-view-mode-map
              (("C-f" . isearch-forward))
	      ))
; This is pretty much a copy from the git page
;https://github.com/fuxialexander/org-pdftools/tree/a5b61bca3f8c91b0859bb0df1a929f9a31a57b99
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;;;Toggle window dedicated
;https://github.com/emacsorphanage/dedicated/blob/master/dedicated.el
(use-package dedicated
  :ensure t
  :bind (("C-s" . dedicated-mode))
  )

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)
)

(use-package dired-open
  :ensure t)


;;; Company autocomplete

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


;;;Latex things

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq define-key LaTeX-mode-map (kbd "C-c a c") 'fa/add-latex-acronym)
   (setq TeX-macro-global
   '("c:/texlive/2020/texmf-var/tex/" "c:/texlive/texmf-local/tex/" "c:/texlive/texmf-local/bibtex/bst/" "c:/texlive/2020/texmf-dist/tex/" "c:/texlive/2020/texmf-dist/bibtex/bst/"))
  (add-hook 'TeX-mode-hook 'TeX-fold-mode)      ; auto-activate TeX-fold-mode
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)  ; auto-activate math mode
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq latex-run-command "pdflatex")
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")); "If you plan to build PDF files via LaTeX you need to make sure that org-latex-pdf-process is set to process the bibliography (using bibtex or biblatex). Here is one example of how to do that (see ./org-ref.org::*LaTeX export for other alternatives)."  (setq org-latex-pdf-process '("latexmk -shell-escape -bibtex -f -pdf %f"))
  (add-hook 'LaTeX-mode-hook (lambda ()
                 (push 
                  '("Latex_outdir" "%`pdflatex --output-directory=/tmp %(mode)%' %t" 
                TeX-run-TeX nil (latex-mode doctex-mode) 
                :help "Run pdflatex with output in /tmp")
                  TeX-command-list)))
   (setq org-latex-default-packages-alist
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
     ("" "acronym" t nil))))

(use-package bibtex-completion
  :ensure t)


(with-eval-after-load 'ox-latex ; add cls files
   (add-to-list 'org-latex-classes
                '("mitthesis"
                  "\\documentclass{mitthesis}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
   (add-to-list 'org-latex-classes
                '("tobysanswers"
                  "\\documentclass{tobysanswers}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
   (add-to-list 'org-latex-classes
                '("final_report"
                  "\\documentclass{final_report}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


; Add acronym from current position https://florian.adamsky.it/2018/03/09/emacs-add-acronyms.html 
(defun fa/add-latex-acronym (region-beg region-end)
  "This function reads the written out form of an acronym via the
minibuffer and adds it to the acronym list in a latex
document. Addtionally, it sorts all acronyms in the list."
  (interactive "r")
  (save-excursion
    (let ((acronym
           (if (region-active-p)
               (buffer-substring region-beg region-end)
             (read-from-minibuffer "Acronym: ")))
          (full-name (read-from-minibuffer "Full Name: ")))
      (beginning-of-buffer)
      (if (search-forward "\\begin{acronym}" nil t)
          (progn
            (deactivate-mark)
            (open-line 1)
            (forward-line 1)
            (insert (concat "  \\acro{" acronym "}{" full-name "}"))
            (beginning-of-line)
            (sort-lines nil (point) (search-forward "\\end{acronym}" nil nil)))
        (user-error "No acronym environment found")))))


;;;org-brain config allows for structured thought

;https://github.com/Kungsgeten/org-brain
(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/entropy/brain")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/cache/data/org/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

;;;Polymode allows multiple major modes 
;https://polymode.github.io/defining-polymodes/
(use-package polymode
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode)
(with-eval-after-load "polymode"
  (define-hostmode org-brain-poly-hostmode
    :mode 'org-brain-visualize-mode)))


