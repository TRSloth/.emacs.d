# My Emacs Config

My Emacs Config allows you to view my notes as I see then.

My notes should be fairly self-evident though I use a range of Emacs tools and packages to give them structure and help form ideas in my brain.  This config will allow you to see my notes as I see them(with any luck).


## Prerequisites

Before you begin, ensure you have met the following requirements:
<!--- These are just example requirements. Add, duplicate or remove as required --->
* You have installed the latest version of [Emacs](https://www.gnu.org/software/emacs/)
* You have a Windows 10 (64bit) machine. I've also done built some of the configuration for Linux though this is incomplete, so I can't guarentee it will work.

## Installing My Emacs Config

To install My Emacs Config, follow these steps:

Windows:
```
mklink /J C:\Users\<YourName>\AppData\Roaming\.emacs.d C:\path\to\this\repo
```
## Using My Emacs Config

To use My Emacs Config, follow these steps:
1. Press <Win>+R
2. Type "C:\Program Files\Emacs\bin\runemacs.exe"
3. Press enter.
4. Done, all packages should be installed by use-package :)
```

## Contributors

Thanks all the people who provided the guides howto's and software to make building a config that works for me possible, to name a few:

* [@nobiot](https://github.com/nobiot) 🐛 [Zero-to-Emacs-and-Org-Roam](https://github.com/nobiot/Zero-to-Emacs-and-Org-roam)
* [@scottydocs](https://github.com/scottydocs) 📖


## Contact

If you want to contact me you can reach me at tobyarowlands@gmail.com.

## Why have you done the same thing twice

Good question, story short I needed to rebuild my config with use-package to allow for auto-installation on multiple machiences and I heard prelude was useful for this as everything was in one directory.  However, after getting most of it up and running it felt sluggish and I was having some issues with keybinds. So I decided to transition back to stock emacs using .gitignore to not version control the programs that will install themself's.

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