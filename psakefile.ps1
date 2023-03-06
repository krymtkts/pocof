Properties {
    if (-not $Stage) {
        $Stage = 'Debug'
    }
    if ($DryRun -eq $null) {
        $DryRun = $true
    }
    $ModuleName = Resolve-Path ./src/*/*.psd1 | Split-Path -LeafBase
    $ModuleVersion = (Resolve-Path "./src/${ModuleName}/*.fsproj" | Select-Xml '//Version/text()').Node.Value
    $ModuleRoot = Split-Path -Parent $PSCommandPath
    "Module: $ModuleName ver$ModuleVersion root=$ModuleRoot"
}

Task default -depends TestAll

Task TestAll -depends Init, Build, UnitTest, Test

Task Init {
    'Init is running!'
}

Task Clean {
    'Clean is running!'
    Get-Module pocof -All | Remove-Module -Force -ErrorAction SilentlyContinue
    Remove-Item .\src\*\bin -Recurse -Force -ErrorAction SilentlyContinue
    Remove-Item .\src\*\obj -Recurse -Force -ErrorAction SilentlyContinue
    Remove-Item .\release -Recurse -Force -ErrorAction SilentlyContinue
}

Task Build -depends Clean {
    'Build command let!'
    Import-LocalizedData -BindingVariable module -BaseDirectory (Resolve-Path "./src/${ModuleName}/") -FileName "$ModuleName.psd1"
    if ($module.ModuleVersion -ne (Resolve-Path "./src/*/${ModuleName}.fsproj" | Select-Xml '//Version/text()').Node.Value) {
        throw 'Module manifest (.psd1) version does not match project (.fsproj) version.'
    }
    dotnet publish -c $Stage
    "Completed to build $ModuleName ver$ModuleVersion"
}

Task UnitTest {
    dotnet test --collect:"XPlat Code Coverage" --nologo
}

Task Coverage -depends UnitTest {
    reportgenerator -reports:'.\src\pocof.Test\TestResults\*\coverage.cobertura.xml' -targetdir:'coverage' -reporttypes:Html
}

Task Import -depends Build {
    "Import $ModuleName ver$ModuleVersion"
    if ( -not ($ModuleName -and $ModuleVersion)) {
        throw "ModuleName or ModuleVersion not defined. $ModuleName, $ModuleVersion"
    }
    switch ($Stage) {
        'Debug' {
            Import-Module (Resolve-Path ./src/*/bin/Debug/*/publish/*.psd1) -Global
        }
        'Release' {
            $installPath = Join-Path ($env:PSModulePath -split ';' -like '*\Users\*') $ModuleName -AdditionalChildPath $ModuleVersion
            Copy-Item (Resolve-Path "./src/${ModuleName}/bin/Release/*/publish/*") $installPath -Verbose -Force
            Import-Module $ModuleName -Global
        }
    }
}

Task Test -depends Import {
    Invoke-Pester
}

Task ExternalHelp -depends Import {
    if (-not (Test-Path ./docs)) {
        New-MarkdownHelp -Module pocof -OutputFolder ./docs
    }
    New-ExternalHelp docs -OutputPath (Resolve-Path "./src/${ModuleName}/") -Force
}

Task Release -precondition { $Stage -eq 'Release' } -depends Test, ExternalHelp {
    "Release $($ModuleName)! version=$ModuleVersion dryrun=$DryRun"

    $m = Get-Module $ModuleName
    if ($m.Version -ne $ModuleVersion) {
        throw "Version inconsistency between project and module. $($m.Version), $ModuleVersion"
    }
    $RequiredVersion = "$($m.Version)$(if ($m.PrivateData.PSData.Prerelease) {'-' + $m.PrivateData.PSData.Prerelease } else {''})"

    $Params = @{
        Name = $ModuleName
        NugetAPIKey = (Get-Credential API-key -Message 'Enter your API key as the password').GetNetworkCredential().Password
        WhatIf = $DryRun
        Verbose = $true
        AllowPrerelease = $true
        RequiredVersion = $RequiredVersion
    }
    Publish-Module @Params
}