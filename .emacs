;;;; Customise this file to match your directory structure
; Place this file in the location emacs expects the system init file to be, or symlink to it.
; Personally I use symlinks to keep everything in the same file.
;
;;; Windows 
;; Expected location(Vista and up):
; C:/Users/<Username>/AppData/Roaming/
;; Symlink:
; mklink C:\Users\<username>\AppData\Roaming\.emacs C:\<path-to-repo>\.emacs
;
;;; Linux
;; Expected location:
; 
;; Symlink:
; ls -s /path/to/this/repo/.emacs ~/.emacs
;
;;; Guidance
; Suggested changes are commented "CHANGEME", other suggested changes may be found in options.el


;;;; Set Home path

;;; Windows
(when (string-equal system-type "windows-nt")
  (setenv "HOME" "G:")	       				; MUST, IF using Windows, set as path to this folder
)

;;; Linux 
(when (string-equal system-type "gnu/linux")
  (setenv "HOME" "/home/chaos")		      		; MUST, change if using LINUX to the parent dir of this directory
)



;;;; Set and start init

;;; Set init dir to repo-name, or if that doesn't exist check the default name
(setq user-emacs-directory (locate-user-emacs-file "~/dot-emacs/" ".emacs.d/"))


;;; Set init file name
; Set so emacs doesn't reload this file(.emacs) if you went for the symlink approach
(setq user-init-file (concat user-emacs-directory "init"))		

;;; Load init file
(setq load-prefer-newer t)				;prefer the newest version of the file (.el or .elc)
(load user-init-file)
