#
# Script module for module 'pocof'
# Based on https://learn.microsoft.com/en-us/powershell/gallery/concepts/module-psedition-support?view=powershellget-3.x#targeting-multiple-editions
#
Set-StrictMode -Version Latest

$PSModule = $ExecutionContext.SessionState.Module
$PSModuleRoot = $PSModule.ModuleBase

# NOTE: Import the appropriate nested binary module based on the current PowerShell version.
# https://learn.microsoft.com/en-us/powershell/scripting/install/powershell-support-lifecycle?view=powershell-7.5#powershell-end-of-support-dates
if ($PSVersionTable.PSVersion -lt [Version]'7.2') {
    $targetFramework = 'netstandard2.0'
}
else {
    $targetFramework = 'net6.0'
}
# NOTE: Build paths separately for Windows PowerShell compatibility.
$binaryModuleRoot = Join-Path -Path $PSModuleRoot -ChildPath $targetFramework
$binaryModulePath = Join-Path -Path $binaryModuleRoot -ChildPath 'pocof.dll'
$binaryModule = Import-Module -Name $binaryModulePath -PassThru

Write-Verbose "pocof: Loaded $targetFramework binary from $binaryModulePath" -Verbose:($VerbosePreference -ne 'SilentlyContinue')

# NOTE: When the module is unloaded, remove the nested binary module that was loaded with it.
$PSModule.OnRemove = {
    Remove-Module -ModuleInfo $binaryModule
}
