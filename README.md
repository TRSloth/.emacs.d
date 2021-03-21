# .emacs.d
 My Emacs config


###Setup on windows
mklink /J C:\Users\TobyR\AppData\Roaming\.emacs.d G:\.emacs.d 

###Setup on linux
ls -s /gdrive/.emacs.d ~/.emacs.d

##Additional setup
#Compile .el(lisp) files into lisp bytecode
#https://stackoverflow.com/a/1217249
C-u 0 M-x byte-recompile-directory ~/.emacs.d/
