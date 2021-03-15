;;; Emacs key check order minor mode > local keys(major mode) > global keys
;;; For help see: https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;;; Commands are things that may be invoked with M-x 
;;; Non-interactive functions(not found with M-x cannot be added to a key)
;;; To invoke a cpmmad with params it must be wrapped in a lamda or defun
;------------------------------------------------------------------------------------------

;;; Minor mode keymaps
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward); remap to fix issue fixed by above
 ; (define-key pdf-view (kbd "C-s") 'isearch-forward) ;counsel search not work in pdf view
; (define-key pdf-view-mode-map (kbd "y") 'org-noter-insert-selected-text-inside-note-content)

;------------------------------------------------------------------------------------------

;;; local (Major Mode) keymaps
(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "C-c a c") 'fa/add-latex-acronym))
(eval-after-load "org"
  '(define-key org-mode-map (kbd "C-c a c") 'fa/add-latex-acronym))
(define-key helm-command-map "b" 'helm-bibtex)
(define-key helm-command-map "B" 'helm-bibtex-with-local-bibliography)
(define-key helm-command-map "n" 'helm-bibtex-with-notes)
(define-key helm-command-map "h" 'helm-resume)


;;; Globally defined keys
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-c l p") #'latex-preview-pane-mode)
(global-set-key (kbd "C-c c") #'org-ref-helm-insert-cite-link)
(global-set-key (kbd "C-c n r") #'org-roam-buffer-toggle-display)
(global-set-key (kbd "C-c n i") #'org-roam-insert)
(global-set-key (kbd "C-c n u") #'org-roam-insert-immediate)
(global-set-key (kbd "C-c n .") #'org-roam-find-file-other-window)
(global-set-key (kbd "C-c n /") #'org-roam-find-file)
(global-set-key (kbd "C-c n b") #'org-roam-switch-to-buffer)
(global-set-key (kbd "C-c n d") #'org-roam-find-directory)
(global-set-key (kbd "C-c n l") #'org-insert-link-global)
(global-set-key (kbd "C-c n o") #'org-noter)
(global-set-key (kbd "C-c n c") #'orb-insert);make note for a citations
(global-set-key (kbd "C-c n t") #'delete-file)

;;; Copy paste remaps
(global-set-key "\C-v" 'clipboard-yank)
(global-set-key "\M-v" 'yank-pop)
(global-set-key "\C-w" 'kill-ring-save)
(global-set-key "\M-w" 'kill-region)
(global-set-key "\C-c k" #'cua-mode)
(global-set-key (kbd "C-z") 'undo) ;Emacs default is bound to hide Emacs.
;;; change window size
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Replace defaults with Counsel and swiper versions
(global-set-key (kbd "M-x") #'counsel-M-x)
(global-set-key (kbd "C-x C-f") #'counsel-find-file)
(global-set-key (kbd "C-c n f") #'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-c M-f") #'counsel-recentf)
(global-set-key (kbd "C-f") #'swiper)
(global-set-key (kbd "C-C C-f") 'isearch-forward) ; just in case its still needed(pdf-viewer etc)
;;; Paste bin for regualterly used strings
(global-set-key (kbd "C-c a a") "#+roam_alias: ")
(global-set-key (kbd "C-c a s") "#+startup: content\n")
(global-set-key (kbd "C-c a t") "#+roam_tags: ")
(global-set-key (kbd "C-c a b") "#+roam_tags:  \"BT\" \n")

;;; Defined in commands
(global-set-key (kbd "C-s") 'toggle-window-dedicated)
(global-set-key (kbd "C-c e") #'find-user-init-file)
