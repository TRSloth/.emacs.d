;;; ------Link files in Windows-----
(when (string-equal system-type "windows-nt")
  (setq user-emacs-directory "g:/.emacs.d/") ;;needs to specify full-path
  (setq default-directory "~/gdrive")
  (setenv "HOME" "G:")
  (setq org-roam-graph-executable "~/.emacs.d/win/Graphviz/bin/dot.exe")
;;; ------Find linux functions in windows-----
  (add-to-list 'exec-path "~/.emacs.d/win") ;; for any additional files needed by windows
  (setenv "PATH"
    (concat;mysys prefered over cygwin i
     "C:/msys64/mingw64/bin" ";"
     "C:/texlive/2020/bin/win32" ";" ;;PDFlatex
     "C:/cygwin64/usr/local/bin" ";"
     "C:/cygwin64/usr/sbin" ";"
     "C:/cygwin64/bin" ";"
     "C:/Program Files/Git/bin" ";" ;;All already included
     (getenv "PATH"))))

;;; ------Link files in Linux -----
(when (string-equal system-type "gnu/linux")
  (setq user-emacs-directory "/home/chaos/.emacs.d/")
  (setq default-directory "~/gdrive")
  (setenv "HOME" "/home/chaos"))
