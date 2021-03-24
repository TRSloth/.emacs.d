# My Emacs Config

Emacs Config preconfigured for org-roam(including server), org-brain, pdftools and a range of bibleography and note-taking tools.

## Prerequisites

Before you begin, ensure you have met the following requirements:
<!--- These are just example requirements. Add, duplicate or remove as required --->
* You have installed the latest version of [Emacs](https://www.gnu.org/software/emacs/)
* Windows 10 (64bit) or Kali(Linux), should work on other OS's but those are the ones I've tested.

## Installing My Emacs Config

[Clone](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository) or [download](https://github.com/TRSloth/emacs-old/archive/refs/heads/main.zip) this repository

Then to install the repository, follow these steps:
### Windows:
1. Create a symlink from where emacs [expects to find](https://superuser.com/questions/137971/where-is-the-emacs-file-located-on-windows) the .emacs.d file to where you've placed it, or move it to the correct location.
```
mklink /J C:\Users\<YourName>\AppData\Roaming\.emacs.d C:\path\to\this\repo
```
2. Press `<Win>+R`
2. Type `"C:\Program Files\Emacs\bin\runemacs.exe"` + RET 
4. Use-package will start installing things: Go make a cup of tea/coffee
5. Delete `org-brain.elc` (in `.emacs.d/elpa/org-brain-xxxx/`), this fixes a [known issue](https://github.com/Kungsgeten/org-brain/issues/320)
6. Right-click and select edit on `.emacs.d/win/org-protocols.reg` and check the path is correct then double-click the program to run. 
7. Done :)

### Linux:
1. Create a symlink from where emacs [expects to find](https://www.emacswiki.org/emacs/DotEmacsDotD) this file to where you've placed it, or move itto the correct location.
```
ls -s /path/to/this/repo/.emacs.d ~/.emacs.d
```
2. Enter `<Win>` + `"Emacs"` + RET
3. Use-package will start installing things
4. When asked about installing packages answer `yes`(these were included for windows as they can't auto-install)
5. Delete `org-brain.elc` (in `.emacs.d/elpa/org-brain-xxxx/`), this fixes a [known issue](https://github.com/Kungsgeten/org-brain/issues/320)
6. Done :)

## Contributors

Thanks all the people who provided the guides howto's and software to make building a config that works for me possible, to name a few:
* If I've copied any code from anywhere I've linked to it directly above the code in a comment
* [@nobiot](https://github.com/nobiot) 🐛 [Zero-to-Emacs-and-Org-Roam](https://github.com/nobiot/Zero-to-Emacs-and-Org-roam)
* [@scottydocs](https://github.com/scottydocs) 📖


## Contact

If you want to contact me you can reach me at tobyarowlands@gmail.com.

## License
<!--- If you're not sure which open license to use see https://choosealicense.com/--->

This project uses the following license:
MIT License

Copyright (c) 2021 Toby Rowlands

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
