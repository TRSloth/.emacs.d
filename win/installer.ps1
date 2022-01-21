### Todo
# Add notification what software is installed/give feedback when done
# Add option to add newley installed programs to taskbar
# Set-ExecutionPolicy -ExecutionPolicy unrestricted -Scope CurrentUser
$scriptPath = split-path -parent $MyInvocation.MyCommand.Definition
$programdir=“C:\Users\$env:username\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\"

if (!(Test-Path "C:\msys64"))
{
    # Download the archive
    (New-Object System.Net.WebClient).DownloadFile('https://github.com/msys2/msys2-installer/releases/download/2021-07-25/msys2-base-x86_64-20210725.sfx.exe', $scriptPath+'\msys2.exe')  #Could also use latest if set url https://github.com/msys2/msys2-installer/releases/latest/download/msys2-base-x86_64-20211130.sfx.exe
    .\msys2.exe -y -oC:\  # Extract to C:\msys64
    Remove-Item msys2.exe  # Delete the archive again
    # Run for the first time
    C:\msys64\usr\bin\bash -lc 'pacman msys2-keyring -S --noconfirm' #install approved keys
    # Make a program link
    .\shortcutmarker.ps1
} 
else 
{
    Write-Output "msys2 detected, install skipped"
}

# Update MSYS2
Write-Output "Updating msys2 packages..."
#C:\msys64\usr\bin\bash -lc 'pacman -Syuu --needed --noconfirm --quiet'  # Core update (in case any core packages are outdated)
#$env:CHERE_INVOKING = 'yes'  # Preserve the current working directory
#$env:MSYSTEM = 'MINGW64'  # Start a 64 bit Mingw environment
C:\msys64\usr\bin\bash -lc 'pacman -Syuu --needed --noconfirm --quiet base-devel mingw-w64-x86_64-toolchain mingw-w64-x86_64-zlib mingw-w64-x86_64-libpng mingw-w64-x86_64-poppler mingw-w64-x86_64-imagemagick mingw64/mingw-w64-x86_64-aspell mingw64/mingw-w64-x86_64-aspell-en'

# todo add checks to use windows version if installed
C:\msys64\usr\bin\bash -lc 'pacman  -S --needed --noconfirm --quiet git'

# Install Emacs
$wininstall = "C:\Program Files\Emacs\x86_64\bin\runemacs.exe"
$launchinstall = "C:\msys64\mingw64\bin\runemacs.exe"
$regval = "`"C:\msys64\mingw64\bin\emacsclientw.exe`" `"%1`""
# wsl version: "`"C:\msys64\mingw64\bin\wsl.exe`" emacsclient `"%1`""
$dotemacsPath = (split-path -parent $MyInvocation.MyCommand.Definition) | Split-Path #path just above where this file is executing(where .emacs is located)
$currentUserName = [System.Security.Principal.WindowsIdentity]::GetCurrent().Name.split("\")[1]


if ((Test-Path $wininstall) -and ((Read-Host 'Emacs detected, overwrite the current config? (Y/N) if no emacs will proceed in the msys2 environment') -eq 'Y'))
{ #make symlink to windows version
    
        $userdir= Join-Path -Path "c:\Users" -ChildPath $currentUserName"\AppData\Roaming" -Resolve
        $launchinstall = $wininstall
        $regval = "`"C:\Program Files\Emacs\x86_64\bin\emacsclientw.exe`" `"%1`""
}else {
        C:\msys64\usr\bin\bash -lc 'pacman -S --needed --noconfirm --quiet mingw-w64-x86_64-emacs'
        $userdir= Join-Path "c:\msys64\home*" $currentUserName -Resolve
        # Make a program link
        $Shortcut = $WshShell.CreateShortcut($programdir+"emacs.lnk")
        $Shortcut.TargetPath = "C:\msys64\mingw64\bin\runemacs.exe"
        $Shortcut.Save()
}


if (Test-Path $userdir"\.emacs"){
    $creationTime = (gi $userdir"\.emacs").CreationTime.ToString('yyMMddHHmmss') 
    Rename-Item -Force -Path $userdir"\.emacs" -NewName $creationTime".emacs"
}

cmd /c mklink $userdir\.emacs  $dotemacsPath\.emacs

# Add registry values for org-protocol
New-PSDrive -Name HKCR -PSProvider Registry -Root HKEY_CLASSES_ROOT
New-Item -Path "HKCR:\org-protocol" -ErrorAction SilentlyContinue | Out-Null
Set-ItemProperty -Path "HKCR:\org-protocol" -Name "(Default)" -Type string -Value "URL:Org Protocol"
Set-ItemProperty -Path "HKCR:\org-protocol" -Name "URL Protocol" -Type String -Value "" -Force
New-Item -Path "HKCR:\org-protocol\shell" -ErrorAction SilentlyContinue | Out-Null
New-Item -Path "HKCR:\org-protocol\shell\open" -ErrorAction SilentlyContinue | Out-Null
New-Item -Path "HKCR:\org-protocol\shell\open\command" -ErrorAction SilentlyContinue | Out-Null
Set-ItemProperty -Path "HKCR:\org-protocol\shell\open\command" -Name "(Default)" -Type string -Value $regval

Start-Process $launchinstall
Read-Host -Prompt "Install complete, please review any errors above and Press Enter to exit"
# Maybe edit this to detect if error ocurred