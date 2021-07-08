# Download the archive
(New-Object System.Net.WebClient).DownloadFile('https://github.com/msys2/msys2-installer/releases/download/2020-06-29/msys2-base-x86_64-20200629.sfx.exe', 'msys2.exe')
.\msys2.exe -y -oC:\  # Extract to C:\msys64
Remove-Item msys2.exe  # Delete the archive again
# Run for the first time
C:\msys64\usr\bin\bash -lc ' '
# Update MSYS2
C:\msys64\usr\bin\bash -lc 'pacman --noconfirm -Syuu'  # Core update (in case any core packages are outdated)
C:\msys64\usr\bin\bash -lc 'pacman --noconfirm -Syuu'  # Normal update
$env:CHERE_INVOKING = 'yes'  # Preserve the current working directory
$env:MSYSTEM = 'MINGW64'  # Start a 64 bit Mingw environment
C:\msys64\usr\bin\bash -lc 'pacman -Syu --noconfirm'
C:\msys64\usr\bin\bash -lc 'pacman -S base-devel --noconfirm'
C:\msys64\usr\bin\bash -lc 'pacman -S mingw-w64-x86_64-toolchain --noconfirm'
C:\msys64\usr\bin\bash -lc 'pacman -S mingw-w64-x86_64-zlib --noconfirm'
C:\msys64\usr\bin\bash -lc 'pacman -S mingw-w64-x86_64-libpng --noconfirm'
C:\msys64\usr\bin\bash -lc 'pacman -S mingw-w64-x86_64-poppler --noconfirm'
C:\msys64\usr\bin\bash -lc 'pacman -S mingw-w64-x86_64-imagemagick --noconfirm'
C:\msys64\usr\bin\bash -lc 'pacman -S mingw-w64-x86_64-emacs --noconfirm'
C:\msys64\usr\bin\bash -lc 'pacman -S mingw64/mingw-w64-x86_64-aspell --noconfirm' #install aspell
C:\msys64\usr\bin\bash -lc 'pacman -S mingw64/mingw-w64-x86_64-aspell-en --noconfirm'
C:\msys64\usr\bin\bash -lc 'pacman -S git --noconfirm'#msys2 struggles locating git so attempt install just in case
#symlink config, had issues with powershell symlinks to I use command prompt
#customise these paths for your install

#run regedit
regedit /s .\dont-run.reg


$CurrentUserName = [System.Security.Principal.WindowsIdentity]::GetCurrent().Name.split("\")[1]
$userdir= Join-Path "c:\msys64\home*" $CurrentUserName -Resolve
Set-Location -Path $userdir
cmd /c mklink .emacs  C:\repo\IndividualProject_2020_Toby-Rowlands\dot-emacs\.emacs #had issues with powershell symlink

C:/msys64/mingw64/bin/runemacs.exe #run emacs
