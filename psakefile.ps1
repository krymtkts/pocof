Properties {
    if (-not $Stage) {
        $Stage = 'Debug'
    }
    if ($DryRun -eq $null) {
        $DryRun = $true
    }
    $ModuleName = Resolve-Path ./src/*/*.psd1 | Split-Path -LeafBase
    $ModuleVersion = (Resolve-Path "./src/${ModuleName}/*.fsproj" | Select-Xml '//Version/text()').Node.Value
    $ModuleSrcPath = Resolve-Path "./src/${ModuleName}/"
    "Module: $ModuleName ver$ModuleVersion root=$ModuleSrcPath"
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
    Import-LocalizedData -BindingVariable module -BaseDirectory $ModuleSrcPath -FileName "$ModuleName.psd1"
    if ($module.ModuleVersion -ne (Resolve-Path "./src/*/${ModuleName}.fsproj" | Select-Xml '//Version/text()').Node.Value) {
        throw 'Module manifest (.psd1) version does not match project (.fsproj) version.'
    }
    dotnet publish -c $Stage
    "Completed to build $ModuleName ver$ModuleVersion"
}

Task UnitTest {
    Remove-Item .\src\pocof.Test\TestResults\* -Recurse -ErrorAction SilentlyContinue
    dotnet test --collect:"XPlat Code Coverage" --nologo
}

Task Coverage -depends UnitTest {
    Remove-Item .\coverage\*
    reportgenerator -reports:'.\src\pocof.Test\TestResults\*\coverage.cobertura.xml' -targetdir:'coverage' -reporttypes:Html
}

Task WorkflowTest {
    if (-not (Get-Command act -ErrorAction SilentlyContinue)) {
        throw 'act is not installed. Read https://github.com/nektos/act and install it.'
    }
    act pull_request --verbose --platform ubuntu-latest=catthehacker/ubuntu:pwsh-latest
}

Task Import -depends Build {
    "Import $ModuleName ver$ModuleVersion"
    if ( -not ($ModuleName -and $ModuleVersion)) {
        throw "ModuleName or ModuleVersion not defined. $ModuleName, $ModuleVersion"
    }
    switch ($Stage) {
        'Debug' {
            Import-Module (Resolve-Path "${ModuleSrcPath}/bin/Debug/*/publish/*.psd1") -Global
        }
        'Release' {
            $installPath = Join-Path ($env:PSModulePath -split ';' -like '*\Users\*') $ModuleName -AdditionalChildPath $ModuleVersion
            Copy-Item (Resolve-Path "${ModuleSrcPath}/bin/Release/*/publish/*") $installPath -Verbose -Force
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
    New-ExternalHelp docs -OutputPath $ModuleSrcPath -Force
}

Task Release -precondition { $Stage -eq 'Release' } -depends Test, ExternalHelp {
    "Release $($ModuleName)! version=$ModuleVersion dryrun=$DryRun"

    $m = Get-Module $ModuleName
    if ($m.Version -ne $ModuleVersion) {
        throw "Version inconsistency between project and module. $($m.Version), $ModuleVersion"
    }

    $Params = @{
        Path = $m.Path
        Repository = 'PSGallery'
        ApiKey = (Get-Credential API-key -Message 'Enter your API key as the password')
        WhatIf = $DryRun
        Verbose = $true
    }
    Publish-PSResource @Params
}