
$WshShell = New-Object -comObject WScript.Shell
$scriptPath = split-path -parent $MyInvocation.MyCommand.Definition
$programdir=“C:\Users\$env:username\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\"
$msysdir=$programdir+"MSYS2 64bit\”
    [system.io.directory]::CreateDirectory($msysdir)
    $Shortcut = $WshShell.CreateShortcut($msysdir+"MSYS2 MinGW Clang x64.lnk")
    $Shortcut.IconLocation = "c:/msys64/clang64.exe"
    $Shortcut.Arguments = "-clang64"
    $Shortcut.TargetPath = """C:\msys64\msys2_shell.cmd""" 
    $Shortcut.WorkingDirectory = '"C:\msys64"'
    $Shortcut.Save()
    $Shortcut = $WshShell.CreateShortcut($msysdir+"MSYS2 MinGW UCRT x64.lnk")
    $Shortcut.IconLocation = "c:/msys64/ucrt64.exe"
    $Shortcut.Arguments = "-ucrt64"
    $Shortcut.TargetPath = """C:\msys64\msys2_shell.cmd""" 
    $Shortcut.WorkingDirectory = '"C:\msys64"'
    $Shortcut.Save()
    $Shortcut = $WshShell.CreateShortcut($msysdir+"MSYS2 MinGW x64.lnk")
    $Shortcut.IconLocation = "c:/msys64/mingw64.exe"
    $Shortcut.Arguments = "-mingw64"
    $Shortcut.TargetPath = """C:\msys64\msys2_shell.cmd""" 
    $Shortcut.WorkingDirectory = '"C:\msys64"'
    $Shortcut.Save()
    $Shortcut = $WshShell.CreateShortcut($msysdir+"MSYS2 MinGW x86.lnk")
    $Shortcut.IconLocation = "c:/msys64/mingw32.exe"
    $Shortcut.Arguments = "-mingw32"
    $Shortcut.TargetPath = """C:\msys64\msys2_shell.cmd""" 
    $Shortcut.WorkingDirectory = '"C:\msys64"'
    $Shortcut.Save()
    $Shortcut = $WshShell.CreateShortcut($msysdir+"MSYS2 MSYS.lnk")
    $Shortcut.IconLocation = "c:/msys64/msys2.exe"
    $Shortcut.Arguments = "-msys"
    $Shortcut.TargetPath = """C:\msys64\msys2_shell.cmd""" 
    $Shortcut.WorkingDirectory = '"C:\msys64"'
    $Shortcut.Save()

