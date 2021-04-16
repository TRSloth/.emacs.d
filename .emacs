;;;; Customise this file to match your directory structure
; Place this file in the location emacs expects the system init file to be, or symlink to it.
; Personally I use symlinks to keep everything in the same file.
;
;;; Windows 
;; Expected location(Vista and up):
; C:/Users/<Username>/AppData/Roaming/
;; Symlink:
; mklink C:\Users\<username>\AppData\Roaming\.emacs C:\<path-to-repo>\IndividualProject_2020_Toby-Rowlands\dot-emacs\.emacs
;
;;; Linux
;; Expected location:
; 
;; Symlink:
; ls -s /path/to/this/repo/.emacs ~/.emacs
;
;;; Guidance
; Suggested changes are commented "CHANGEME", other suggested changes may be found in options.el
;
;;; OS options from docs find with "C-x c a" system-type
;  ‘gnu’          compiled for a GNU Hurd system.
;  ‘gnu/linux’    compiled for a GNU/Linux system.
;  ‘gnu/kfreebsd’ compiled for a GNU system with a FreeBSD kernel.
;  ‘darwin’       compiled for Darwin (GNU-Darwin, macOS, ...).
;  ‘ms-dos’       compiled as an MS-DOS application.
;  ‘windows-nt’   compiled as a native W32 application.
;  ‘cygwin’       compiled using the Cygwin library.



;;; set user-emacs-directory first or emacs will add one at HOME(set below)
(setq user-emacs-directory "~/dot-emacs/")



;;;; Set Home path
(let ((os system-type)) 
  (cond ((eql os 'windows-nt) (setenv "HOME" "C:"))	; CHANGEME, IF using Windows, set as path to parent dir of this folder
        ((eql os 'gnu/linux) (setenv "HOME" "/home/chaos"))					; CHANGEME, IF using LINUX to the path of parent dir of this directory
  )
)



;;;; Set and start init
;;; Set init dir to repo-name, or if that doesn't exist check the default name
(setq user-emacs-directory (locate-user-emacs-file "~/dot-emacs/" ".emacs.d/"))

;;; Set init file name
; Set so emacs doesn't reload this file(.emacs) if you went for the symlink approach
(setq user-init-file (concat user-emacs-directory "init"))
(setq package-user-dir (concat user-emacs-directory "elpa/"))	

;;; Load init file
(setq load-prefer-newer t)				;prefer the newest version of the file (.el or .elc)
;(load )
(load user-init-file)
