(setq debug-on-error t)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; use-package install
;https://github.com/jwiegley/use-package
;https://jwiegley.github.io/use-package/keywords/
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t);always install not already installed packages

;;;; Quick Explantation on use-package, I'm not entirely clear on this so it may no be accurate
; Use-package manages the configuration and loading of packages, it is not a package manager though we can interace with package managers such as the built in package manager to download apps we don't have using ":ensure t", is also allows us to lazy load speading up initialization time
;;; some of the other commands do as follows
; commands: defer loading till after a bound command is used
; bind: defers loading of package until you call a key that is bound to it(see also bind-keymap)
; mode: load when file loaded contains a matching string(see also magic and interpreter)
; init: execute code before the package is loaded, this could be setting paths or binding commands to lazy load, happens on startup, keep these simple with as much as possible in config
; config: execute code after a package loads(if package loading is defered config is too)
; defer: will defer loading even if not using commands which imply defer, for use when loading from another package 
; hook: add a function to a package hook(hook appends the word hook to be package itself), eg a package will load a function when it is ready for it
; custom: similar to config, but allows code execution when customisations are assigned. (see also custom-face)
; errors in use package will be sent to the warmings buffer



;;;load other lisp
(load "~/.emacs.d/my-paths.el" 'noerror)	; noerror stops an error being thrown if the file is not found
(load "~/.emacs.d/aesthetics.el" 'noerror)
(load "~/.emacs.d/pastebin.el" 'noerror)	; Strings I use frequently usually assigned to "C-c a ~" 


;;;; Startup options
(set-language-environment "UTF-8")
(put 'set-goal-column 'disabled nil)  ; (C-u) C-x C-n 
;;; Functions/Commands(use 1 or -1)
(delete-selection-mode 1)	; Overwrite highlighted text
(auto-save-visited-mode 1)
(electric-indent-mode -1)

;;; Variables(use t and nil), only 1 setq is required
(setq delete-old-versions t
 custom-safe-themes t
 version-control t
 make-backup-files t
 auto-save-default t
 create-lockfiles nil)
(setq backup-directory-alist  '(("." . "~/.emacs.d/file-backups"))
auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-"
image-file-name-extensions '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm" "svg" "pdf" "bmp"))


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
(global-set-key (kbd "C-G") #'abort-recursive-edit)

; change window size
(global-set-key (kbd "M-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-<down>") 'shrink-window)
(global-set-key (kbd "M-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-C C-s") 'isearch-forward)


;;;; Globally used Minor modes

;;; Page break lines: Show page breaks as lines
; https://github.com/purcell/page-break-lines
(use-package page-break-lines
  :diminish
  :init
  (setq page-break-lines-mode t))

;;; Company autocomplete
; https://github.com/company-mode/company-mode
(use-package company
  :defer 3
  :diminish
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

;;; Which key: show avalible key configs from current press
; https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish
  :config
  (which-key-mode 1)
  :custom (which-key-idle-delay 1.2))

;;; Flycheck: 
(use-package flycheck
  :diminish
  :after org
  :hook
  (org-src-mode . disable-flycheck-for-elisp)
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)

  (defun disable-flycheck-for-elisp ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-to-list 'flycheck-checkers 'proselint))

(use-package flycheck-inline
  :config (global-flycheck-inline-mode))

;; ;Toggle window dedicated
; https://github.com/emacsorphanage/dedicated/blob/master/dedicated.el
(use-package dedicated
  :bind (("C-s" . dedicated-mode))
  )

(use-package diminish
 :config
 (diminish 'visual-line-mode))


;;;; Globally used packages


;;; No-littering: Prevents files being put in /.emacs.d/ and allows for seperate custom.el
; https://github.com/emacscollective/no-littering
(use-package no-littering
  :init
  (setq no-littering-var-directory (expand-file-name "cache/data/" user-emacs-directory))
  (setq no-littering-etc-directory (expand-file-name "cache/etc/" user-emacs-directory))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;;; auto-package-update
; https://github.com/rranelli/auto-package-update.el
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))


;;; Dims inactive buffers
;https://github.com/gonewest818/dimmer.el
(use-package dimmer
  :custom (dimmer-fraction 0.3)
  :config (dimmer-mode))

;;; Allow easy buffer swapping
; https://github.com/lukhas/buffer-move
(use-package buffer-move
 :bind(("<C-S-up>" . buf-move-up)
 ("<C-S-down>" . buf-move-down)
 ("<C-S-left>" . buf-move-left)
 ("<C-S-right>" . buf-move-right))
)


;;;; Globally callable packages

;;; navigation -using ivy

;https://github.com/abo-abo/swiper
(use-package ivy
  :diminish
  :init
  (ivy-mode 1) ; enable ivy at startup
  :bind(("C-c C-r" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
 
)
  
;https://github.com/abo-abo/swiper
(use-package swiper
  :bind(("\C-f" . swiper)))

(use-package counsel
  :bind(("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-h f" . counsel-describe-function)
  ("C-x c a" . counsel-apropos)
  ("C-h v" . counsel-describe-variable)
  ("C-c M-f" . counsel-recentf))
)


;;; navigation -using helm(disabled)
;Advantages: is a buffer(appears where you call it), shows recently used
;Disadvantages bloated, couldn't figure roam autocomplete
(use-package helm
  :disabled
  :bind(("M-x" . helm-M-x)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-f" . helm-find-files))
)

;https://github.com/abo-abo/swiper-helm
(use-package swiper-helm
  :disabled
  :bind(("\C-f" . swiper))
)


;;; improvements to isearch
; used when swiper can't be (inside pdf's etc)

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


;;;; Major modes

;;; org-mode(Org)
(use-package org
  :mode (("\\.org$" . org-mode))
  :bind(
    (("C-c n l" . org-insert-link-global)
    ("C-c n /" . org-roam-find-file)) 
    :map org-mode-map(
      ("C-c a c" . fa/add-latex-acronym)
      ("C-c t" . org-toggle-all-links)))
  :config(progn
  (setq org-display-inline-images t)
  (setq org-hide-leading-stars t)
  (setq org-ellipsis "⤵")
  (setq org-image-actual-width '(600))
  (setq org-startup-folded nil)
  (setq org-hide-block-startup nil)
  (setq org-startup-with-inline-images t))
  (defun org-toggle-all-links ()
"run org-toggle inline-images and link-display commands"
(interactive)
(org-toggle-inline-images t);command
(org-toggle-link-display))
)

;;; pdf-view-mode(PDFView)
; https://github.com/politza/pdf-tools
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode);magic modes allow diminishing? 
  :config
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install :no-query :skip-dependencies)
  :hook (pdf-view-mode . pdf-continuous-scroll-mode);this can be installed and auto-updated with quelpa but that took too long to configure last time.
  :bind (:map pdf-view-mode-map
              (("C-f" . isearch-forward))))

;;; dired mode(dired by name)
(use-package dired-open)

;;;Polymode allows multiple major modes 
;https://polymode.github.io/defining-polymodes/
(use-package polymode
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode)
(with-eval-after-load "polymode"
  (define-hostmode org-brain-poly-hostmode
    :mode 'org-brain-visualize-mode)))

;;;Ivy-bibtex creates a virtual buffer populated with bibtex entries 
;https://github.com/tmalsburg/helm-bibtex(same as helm)
(use-package ivy-bibtex
  :bind (("C-c i b" . ivy-bibtex)
       ("C-c i B" . ivy-bibtex-with-local-bibliography))
  :init
  (require 'ivy)
)

;;;Helm-bibtex creates a buffer populate with bibtex entries 
;https://github.com/tmalsburg/helm-bibtex
(use-package helm-bibtex
  :bind (("C-c h b" . helm-bibtex)
       ("C-c h B" . helm-bibtex-with-local-bibliography)
       ("C-c h n" . helm-bibtex-with-notes)
       ("C-c h h" . helm-resume))
  :init
  (require 'helm)
  :config
  (setq bibtex-completion-pdf-symbol "⌘") ;appears if pdf exists
  (setq bibtex-completion-notes-symbol "✎") ;appears if notes exist
)

(use-package tex
  :ensure auctex
  :defer t
  :hook ((TeX-mode . TeX-fold-mode)
  (LaTeX-mode . LaTeX-math-mode))
  :config
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
     ("" "acronym" t nil)))
  :bind (:map LaTeX-mode-map
              (("C-c a c" . fa/add-latex-acronym)))
)


;;;; org-mode-standalone-minor modes


(use-package org-download
  :defer 4
;  :hook(org-mode . org-download)
  :commands (org-download-clipboard org-download-screenshot org-download-yank)
)

;;; Org-superstar: make bullet points look nicer
;https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :after org
  :defer 2
  :init
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  :config
  (setq org-superstar-headline-bullets-list '(9673 9675 9671 10047))
  )

;https://github.com/org-roam/org-roam
(use-package org-roam
  :commands(org-roam-find-file org-roam-find-directory)
  :hook  (after-init . org-roam-mode)
;  :hook  (org-mode . org-roam-mode);works but has issues on first reload see:/issues/1221
  :bind (:map org-roam-mode-map
              (( "C-c n r" . org-roam-buffer-toggle-display))
              :map org-mode-map
              ("C-c n i" . org-roam-insert)
              ("C-c n I" . org-roam-insert-immediate)
              ("C-c n b" . org-roam-switch-to-buffer)
              ("C-c n d" . org-roam-find-directory))
  :config
  (setq org-roam-db-update-method 'immediate)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-graph-viewer nil)
  )

;;; Org-brain hierarchical thought
;https://github.com/Kungsgeten/org-brain
(use-package org-brain 
  :bind-keymap (("C-c b" . org-brain-visualize-mode-map))	;check source for mappings
  :config
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

;;; 
; https://github.com/fuxialexander/org-pdftools
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))


;;;; org-mode-minor-mode-dependant-modes(except org-roam)

;;; Allows for seeing how many references to a citation of label
;https://github.com/jkitchin/org-ref
(use-package org-ref
  :bind (("C-c c" . org-ref-helm-insert-cite-link))
  :init 
  (require 'helm-bibtex)
)

;;; Bibtex completion backend, same for helm and ivy
;https://melpa.org/#/bibtex-completion
(use-package bibtex-completion)


; https://github.com/fuxialexander/org-pdftools
(use-package org-noter-pdftools
;  :after (org-noter org-pdftools)
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

 ;https://github.com/weirdNox/org-noter
(use-package org-noter
  :after org
 ; :hook (org-mode . org-noter-notes-mode-hook)
  :bind(("C-c n o" . org-noter));make notes with place pdf
  :config
  (setq doc-view-continuous t
  doc-view-scale-internally nil
  org-noter-always-create-frame nil)
  (require 'org-noter-pdftools))

;https://github.com/alphapapa/org-rifle
(use-package helm-org-rifle
  :defer t
  :init (require 'helm)
  :bind(:map org-mode-map
    (("C-c h o" . helm-rifle-occur)
    ("C-c h f" . helm-rifle-occur-files)
    ("C-c h r" . helm-org-rifle-org-directory))))


;;;; org-roam-dependant-minor-modes

;;; org-roam-server: allows interactive graph, turn on with Org-roam-server-mode
;https://github.com/org-roam/org-roam-server
;Install guide
;https://github.com/nobiot/Zero-to-Emacs-and-Org-roam/blob/main/90.org-protocol.md
(use-package org-roam-server
  :after org-roam
  :commands(org-roam-server-mode)
  :init
(require 'org-protocol)
(require 'org-roam-protocol)
  :config
  (server-mode 1)
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
        org-roam-server-network-label-wrap-length 20)
)

;https://github.com/org-roam/org-roam-bibtex
(use-package org-roam-bibtex
  :after org-roam
;  :hook (org-roam-mode . org-roam-bibtex-mode)
  :commands (orb-insert)
  :bind (("C-c n c" . orb-insert));make notes on citations
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
)



;;;; Latex things
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
(load "~/.emacs.d/pdf-continuos-scroll-mode.el")
