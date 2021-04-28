
# My Emacs Config

Emacs Config preconfigured for org-roam(including server), org-brain, pdftools and a range of bibleography and note-taking tools.

## Prerequisites

Before you begin, ensure you have met the following requirements:

* You have installed the latest version of [Emacs](https://www.gnu.org/software/emacs/)
* [Windows 10](https://www.microsoft.com/en-gb/software-download/windows10) (64bit) or [Kali](https://www.kali.org/)(Linux), should work on other OS's but those are the ones I've tested.
* (Reccomended) [Msys2](https://www.msys2.org/) which I believe is a compatiblity layer, though if I'm honest I'm not entirely sure.
* (Optional) A [TeX distribution](https://www.latex-project.org/get/), I use [TeXLive](https://www.tug.org/texlive/) as it's cross platform but it's completely upto you. Having a TeX processor allows you to export Org-notes to TeX or PDF while adding typsetting support.

## Installing My Emacs Config

[Clone](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository) or [download](https://github.com/TRSloth/emacs-old/archive/refs/heads/main.zip) this repository

Then to install the repository, follow these steps:

### Windows:
1. Create a symlink from where emacs [expects to find](https://superuser.com/questions/137971/where-is-the-emacs-file-located-on-windows) the .emacs file to where you've placed it, or move it to that expected location.

#### Expected location(Vista and up):
`C:/Users/<Username>/AppData/Roaming/.emacs`

#### Symlink (run in command prompt):

``` shell
mklink C:\Users\<username>\AppData\Roaming\.emacs 
```
2. Open the `.emacs` file with a text editor and change things to match your filestructure, things you need to change will are commented  "MUST" "SHOULD" & "MAY", see also the aesthetics-example.el
3. Enter `<Win>+R` + `"C:\Program Files\Emacs\bin\runemacs.exe"` + RET 
4. Use-package will start installing things: Go make a cup of tea/coffee
5. Delete `org-brain.elc` (in `.emacs.d/elpa/org-brain-xxxx/`), this fixes a [known issue](https://github.com/Kungsgeten/org-brain/issues/320)
6. Right-click and select edit on `.emacs.d/win/org-protocols.reg` and check the path is correct then double-click the program to run. 
7. Done :)

### Linux:

1. Create a symlink from where emacs [expects to find](https://www.emacswiki.org/emacs/DotEmacsDotD) .emacs to where you've placed it, or move it to the correct location.

#### Expected location:

`/home/<username>/.emacs`

#### Symlink
```shell
ln -s /path/to/this/repo/.emacs ~/.emacs
```
2. Open the `.emacs` file with a text editor and change things to match your filestructure, things you need to change will are commented  "MUST" "SHOULD" & "MAY", see also the aesthetics-example.el
3. Enter `<Win>` + `"Emacs"` + RET
4. Use-package will start installing things: Go make a cup of tea/coffee
5. Delete `org-brain.elc` (in `.emacs.d/elpa/org-brain-xxxx/`), this fixes a [known issue](https://github.com/Kungsgeten/org-brain/issues/320)
6. Done :)

## Contributors

Thanks all the people who provided the guides howto's and software to make building a config that works for me possible, to name a few:
* If I've copied any code from anywhere I've linked to it directly above the code in a comment
* [@nobiot](https://github.com/nobiot) 🐛 [Zero-to-Emacs-and-Org-Roam](https://github.com/nobiot/Zero-to-Emacs-and-Org-roam)
* [@scottydocs](https://github.com/scottydocs) 📖


## Contact

If you want to contact me you can reach me at tobyarowlands@gmail.com.
