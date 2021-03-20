;;; Emacs key check order minor mode > local keys(major mode) > global keys
;;; For help see: https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;;; Commands are things that may be invoked with M-x 
;;; Non-interactive functions(not found with M-x cannot be added to a key)
;;; To invoke a cpmmad with params it must be wrapped in a lamda or defun
;------------------------------------------------------------------------------------------

;;; Minor mode keymaps
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward); remap to fix issue fixed by above


;------------------------------------------------------------------------------------------

;;; local (Major Mode) keymaps
(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "C-c a c") 'fa/add-latex-acronym))
(eval-after-load "org"
  '(define-key org-mode-map (kbd "C-c a c") 'fa/add-latex-acronym))



;;; Globally defined keys
(global-set-key (kbd "C-c l p") #'latex-preview-pane-mode)
(global-set-key (kbd "C-c c") #'org-ref-helm-insert-cite-link)
(global-set-key (kbd "C-c n .") #'org-roam-find-file-other-window);todo add func to map?
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
(global-set-key (kbd "M-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-<down>") 'shrink-window)
(global-set-key (kbd "M-C-<up>") 'enlarge-window)


(global-set-key (kbd "C-C C-f") 'isearch-forward) ; just in case its still needed(pdf-viewer etc)
;;; Paste bin for regualterly used strings
(global-set-key (kbd "C-c a a") "#+roam_alias: ")
(global-set-key (kbd "C-c a s") "#+startup: content\n")
(global-set-key (kbd "C-c a t") "#+roam_tags: ")
(global-set-key (kbd "C-c a b") "#+roam_tags:  \"BT\" \n")

;;; Defined in commands
(global-set-key (kbd "C-s") 'toggle-window-dedicated)
(global-set-key (kbd "C-c e") #'find-user-init-file)
