#.SYNOPSIS
#   Sysprep current machine running Windows Server 2008 R2 and Powershell 5.* (providing workarounds introduced by the new Powershell release)
#.DESCRIPTION
#   Script Syspreps current machine. It provides needed workarounds to make Sysprep work on Windows 2008 R2 running Powershell 5.*.
#   SIL workaround: https://msdn.microsoft.com/en-us/powershell/wmf/5.0/limitation_overview
#   RegKey issue workaround: https://windowsserver.uservoice.com/forums/301869-powershell/suggestions/11591262-bug-wmf-5-production-preview-on-windows-server-2
#   Script must be run from elevated Powershell session.
#.PARAMETER ShutdownAfterSysprep
#   Shutdown the machine after the Sysprep
#.PARAMETER DisableAntivirusBeforeSysprep
#   Prevents antivirus from running on VMs based on the sysprepped image, without requiring a reboot.
#   This switch should not be used to prepare the VM template image, since antivirus should be
#   on by default for these (they are used to create the customer VM).
[CmdletBinding()]
Param(
    [switch] $ShutdownAfterSysprep,
    [switch] $DisableAntivirusBeforeSysprep
)
Process {

    $osVersion = [environment]::OSVersion.Version
    $windows2008 = ($osVersion.Major -eq 6) -and ($osVersion.Minor -eq 1)

    #### Disable antivirus if required
    #### Note: On Win8+, Defender cannot be disabled without a reboot (there does not appear to be
    #### a commandlet to cleanly stop defender once it has been disabled).
    if (-not $windows2008 -and $DisableAntivirusBeforeSysprep) {
        $defenderRegPath = "HKLM:\SOFTWARE\Policies\Microsoft\Windows Defender\"
        if (-not (Test-Path $defenderRegPath)) {
            New-Item $defenderRegPath
        }
        New-ItemProperty -Path $defenderRegPath -Name DisableAntiSpyware -Value 1 -PropertyType DWord -Force
    }

    #### Hack needed to fix PowerShell breaking Sysprep
    Write-Output "Apply WMF 5.0 sysprep workaround"

    # The regkey has to be present there on VM boot. Thus need to patch registry before shutdown
    New-ItemProperty -Path "HKLM:\SOFTWARE\Microsoft\Windows\StreamProvider" -Name LastFullPayloadTime -Value 0 -PropertyType DWord -Force
    # Working around PowerShell 5.0 Bugs: https://msdn.microsoft.com/en-us/powershell/wmf/5.0/limitation_overview
    Set-SilLogging -TargetUri https://BlankTarget -CertificateThumbprint 0123456789
    Publish-SilData -ErrorAction SilentlyContinue
    Remove-Item -Recurse $env:SystemRoot\System32\Logfiles\SIL\ -Force -ErrorAction SilentlyContinue

    #### Run sysprep generalization
    $sysprepCommand = "$env:SystemRoot\system32\sysprep\sysprep.exe"
    $sysprepParameters = '/generalize /oobe /quiet'

    if ($ShutdownAfterSysprep) {
        $sysprepParameters += ' /shutdown'
    } else {
        $sysprepParameters += ' /quit'
    }

    if (-not (Test-Path $sysprepCommand)) {
        throw "Failed to locate sysprep command. This is probably due to Wow64 file redirection. Make sure to run VMAgent in the native OS architecture."
    }

    # IMPORTANT NOTE: running sysprep.exe directly would fail when running under Remote Powershell (WinRM).
    # This is because the sysprep generalization process breaks all running network connections including WinRM itself.
    # Subsequently, when the WinRM session is interrupted, all the processes started under that session are automatically terminated.
    # This causes sysprep.exe itself to be killed before completion!
    # As a workaround, we start sysprep.exe through a Windows scheduled task. The drawback is that it's fire and forget: the remote session
    # has no way to detect when sysprep terminates, nor can it retrieve the exit code from sysprep.exe.
    Write-Output "Sysprepping machine"
    $taskname = "fsharplu-sysprep"

    if ($windows2008) {
        Write-Output "Creating scheduled task (Windows 7/Server 2008)"
        # On Windows 2008, New-ScheduledTaskAction does not exist. The alternative
        # `Register-ScheduledJob` is too limited (works only with PS scripts).
        # So we fall back on command `schtasks.exe`.

        # We set the task to run as SYSTEM to avoid having to pass credentials.
        # NOTE: In 83 years from now, the following commands will stop working, at which point
        # we might consider dropping support for Windows 2008 ;-)
        schtasks /CREATE /TN $taskname /RL HIGHEST /F `
                /TR "$sysprepCommand $sysprepParameters" /SC ONCE /ST 00:00 /SD 01/01/2090 `
                /RU SYSTEM

        schtasks /RUN /TN $taskname

        schtasks /DELETE /TN $taskname /F
    } else {
        Write-Output "Creating scheduled task (Windows >=8)"
        $action = New-ScheduledTaskAction -Execute $sysprepCommand -Argument $sysprepParameters
        $principal = New-ScheduledTaskPrincipal -RunLevel "Highest" -UserId 'SYSTEM'
        New-ScheduledTask -Action $action -Principal $principal `
            | Register-ScheduledTask $taskname -Force `
            | Start-ScheduledTask `
            | Unregister-ScheduledTask -Confirm:$false
    }
}