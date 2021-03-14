
;;; Minor mode commands
(defun org-noter-insert-selected-text-inside-note-content ()
  (interactive)
  (progn (setq currenb (buffer-name))
		 (org-noter-insert-precise-note)
		 (set-buffer currenb)
		 (org-noter-insert-note)
		 (org-noter-quote) ) )

; Wrap search if command not found
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;----------------------------------------------------------------------------------

;;; Major mode commands
(defun other-find-org-file ()
  "Org-roam-find-file', in another window."
  (interactive)
  (other-window org-roam-find-file))

(defun org-roam-find-file-other-window (&rest args)
  (interactive)
  (let ((org-roam-find-file-function #'find-file-other-window))
    (apply #'org-roam-find-file args)))

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

;-----------------------------------------------------------------------------------

;;; Global commands
;;; Quick access to init
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))



(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
