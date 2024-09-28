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
    $ModulePublishPath = Resolve-Path "./publish/${ModuleName}/"
    "Module: ${ModuleName} ver${ModuleVersion} root=${ModuleSrcPath} publish=${ModulePublishPath}"
}

Task default -depends TestAll

# NOTE: I don't know why, but if I add Lint before Test, the module import will be doubled.
Task TestAll -depends Init, Build, UnitTest, Test, Lint

Task Init {
    'Init is running!'
    dotnet tool restore
}

Task Clean {
    'Clean is running!'
    Get-Module pocof -All | Remove-Module -Force -ErrorAction SilentlyContinue
    @(
        "./src/*/*/${Stage}"
        './release'
        "${ModulePublishPath}/*"
    ) | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue -Exclude .gitkeep
}

Task Lint {
    dotnet fsharplint lint "${ModuleName}.sln"
    if (-not $?) {
        throw 'dotnet fsharplint failed.'
    }
    dotnet fantomas ./src --check
    if (-not $?) {
        throw 'dotnet fantomas failed.'
    }
    $warn = Invoke-ScriptAnalyzer -Path .\psakefile.ps1 -Settings .\PSScriptAnalyzerSettings.psd1
    if ($warn) {
        throw 'Invoke-ScriptAnalyzer for psakefile.ps1 failed.'
    }
    $warn = Invoke-ScriptAnalyzer -Path .\tests\pocof.Tests.ps1 -Settings .\PSScriptAnalyzerSettings.psd1
    if ($warn) {
        throw 'Invoke-ScriptAnalyzer for pocof.Tests.ps1 failed.'
    }
}

Task Build -depends Clean {
    'Build command let!'
    Import-LocalizedData -BindingVariable module -BaseDirectory $ModuleSrcPath -FileName "${ModuleName}.psd1"
    if ($module.ModuleVersion -ne (Resolve-Path "./src/*/${ModuleName}.fsproj" | Select-Xml '//Version/text()').Node.Value) {
        throw 'Module manifest (.psd1) version does not match project (.fsproj) version.'
    }
    dotnet publish -c $Stage
    "Completed to build $ModuleName ver$ModuleVersion"
}

Task UnitTest {
    Remove-Item ./src/pocof.Test/TestResults/* -Recurse -ErrorAction SilentlyContinue
    dotnet test --collect:"XPlat Code Coverage" --nologo --logger:"console;verbosity=detailed" --blame-hang-timeout 5s --blame-hang-dump-type full
    if (-not $?) {
        throw 'dotnet test failed.'
    }
    Move-Item ./src/pocof.Test/TestResults/*/coverage.cobertura.xml ./src/pocof.Test/TestResults/coverage.cobertura.xml -Force
}

Task Coverage -depends UnitTest {
    Remove-Item ./coverage/*
    reportgenerator -reports:'./src/pocof.Test/TestResults/coverage.cobertura.xml' -targetdir:'coverage' -reporttypes:Html
}

Task WorkflowTest {
    if (-not (Get-Command act -ErrorAction SilentlyContinue)) {
        throw 'act is not installed. Read https://github.com/nektos/act and install it.'
    }
    act pull_request --verbose --platform ubuntu-latest=catthehacker/ubuntu:pwsh-latest
}

Task UbuntuPwsh {
    if (-not (Get-Command docker -ErrorAction SilentlyContinue)) {
        throw 'docker is not installed. Read https://docs.docker.com/engine/install/ and install it.'
    }
    docker build -t ubuntu-dotnet-pwsh .
    if (-not $?) {
        throw 'docker build failed.'
    }
    docker run --rm -it ubuntu-dotnet-pwsh
    if (-not $?) {
        throw 'docker run failed.'
    }
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
            $installPath = Join-Path ($env:PSModulePath -split ';' -like '*Users*') $ModuleName -AdditionalChildPath $ModuleVersion
            New-Item -Path $installPath -ItemType Directory -ErrorAction SilentlyContinue
            $sourcePath = Resolve-Path "${ModuleSrcPath}/bin/Release/*/publish/*"
            Copy-Item $sourcePath $installPath -Verbose -Force
            Copy-Item $sourcePath $ModulePublishPath -Verbose -Force
            Import-Module $ModuleName -Global
        }
    }
}

Task Test -depends Import {
    $result = Invoke-Pester -PassThru
    if ($result.Failed) {
        throw 'Invoke-Pester failed.'
    }
}

Task ExternalHelp -depends Import {
    if (-not (Test-Path ./docs)) {
        New-MarkdownHelp -Module pocof -OutputFolder ./docs
    }
    New-ExternalHelp docs -OutputPath $ModuleSrcPath -Force
}

Task Release -precondition { $Stage -eq 'Release' } -depends TestAll, ExternalHelp {
    "Release $($ModuleName)! version=$ModuleVersion dryrun=$DryRun"

    $m = Get-Module $ModuleName
    if ($m.Version -ne $ModuleVersion) {
        throw "Version inconsistency between project and module. $($m.Version), $ModuleVersion"
    }
    $p = Get-ChildItem "${ModulePublishPath}/*.psd1"
    if (-not $p) {
        throw "Module manifest not found. $($m.ModuleBase)/*.psd1"
    }

    $Params = @{
        Path = $p.FullName
        Repository = 'PSGallery'
        ApiKey = (Get-Credential API-key -Message 'Enter your API key as the password').GetNetworkCredential().Password
        WhatIf = $DryRun
        Verbose = $true
    }
    Publish-PSResource @Params
}
