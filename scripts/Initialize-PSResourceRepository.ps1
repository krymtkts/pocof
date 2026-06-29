[CmdletBinding()]
param (
    [Parameter(Position = 0)]
    [ValidateSet('pwsh', 'powershell')]
    [string]
    $Shell = 'pwsh',
    [Parameter()]
    [switch]
    $InstallPslrm
)

$PSVersionTable

if ($Shell -eq 'powershell') {
    # NOTE: Without -SkipPublisherCheck may fail with PSResourceGet 1.2.
    Install-Module -Name Microsoft.PowerShell.PSResourceGet -Force -Scope CurrentUser -Repository PSGallery -SkipPublisherCheck
}

# NOTE: This is a workaround for PSResourceGet failing to load the repository store, which causes the following error:
# Cannot retrieve the dynamic parameters for the cmdlet. Loading repository store failed: Could not find a part of the path
# '/home/runner/.local/share/PSResourceGet/PSResourceRepository.xml'.
Get-PSResourceRepository -Name PSGallery
Set-PSResourceRepository -Name PSGallery -Trusted -ApiVersion V2
if ($InstallPslrm) {
    $Params = @{
        Name = 'pslrm'
        Prerelease = $true
        Quiet = $true
        Scope = 'CurrentUser'
        Repository = 'PSGallery'
    }
    if ($Shell -eq 'powershell') {
        # NOTE: SkipDependencyCheck avoids that self-reinstall conflict.
        $Params['SkipDependencyCheck'] = $true
    }
    Install-PSResource @Params
}
