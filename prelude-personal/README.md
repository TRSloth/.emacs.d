# My Emacs Prelude Config

My Emacs Prelude Config allows you to view my notes as I see then.

My notes should be fairly self-evident though I use a range of Emacs tools and packages to give them structure and help form ideas in my brain.  This config will allow you to see my notes as I see them(with any luck).


## Prerequisites

Before you begin, ensure you have met the following requirements:
<!--- These are just example requirements. Add, duplicate or remove as required --->
* You have installed the latest version of [Emacs](https://www.gnu.org/software/emacs/)
* You have installed [Prelude](https://github.com/bbatsov/prelude/), the easiest way to do this is to clone the repository and place the contents in the .emacs.d file (This is located at c:/Users/<YourName>.AppData/Roaming), I typically replace this folder with a [symlink](https://www.howtogeek.com/howto/16226/complete-guide-to-symbolic-links-symlinks-on-windows-or-linux/) the config I with to use allowing for use of multiple configurations.
* Prelude also has a numer of optional [prerequisites](https://prelude.emacsredux.com/en/latest/installation/) which for just viewing my note's I wouldn't worry about, though if I do convert you to emacs their work considering.
* You have a Windows 10 (64bit) machine. I've also done built some of the configuration for Linux though this is incomplete, so I can't guarentee it will work.

## Installing My Emacs Prelude Config

To install My Emacs Prelude Config, follow these steps:

Windows:
```
mklink /J C:\Users\<YourName>\AppData\Roaming\.emacs.d\personal C:\path\to\NotesPrelude
```
## Using My Emacs Prelude Config

To use My Emacs Prelude Config, follow these steps:
1. Press <Win>+R
2. Type "C:\Program Files\Emacs\bin\runemacs.exe"
3. Press enter.
4. Press <Alt>+x and type "package-install" RET "use-package" RET
5. Press <Ctrl>-x <Ctrl>-f and navigate to ~\.emacs.d\personal\.dir-locals.el and add your values.
6. Done, all other packages should be installed by use-package :)
```

## Contributors

Thanks all the people who provided the guides howto's and software to make building a config that works for me possible, to name a few:

* [@nobiot](https://github.com/nobiot) 🐛 [Zero-to-Emacs-and-Org-Roam](https://github.com/nobiot/Zero-to-Emacs-and-Org-roam)
* [Prelude](https://github.com/bbatsov/prelude/graphs/contributors) 🐛
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