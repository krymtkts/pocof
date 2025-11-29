Properties {
    if (-not $Stage) {
        $Stage = 'Debug'
    }
    if ($DryRun -eq $null) {
        $DryRun = $true
    }
    $ModuleName = Get-ChildItem ./src/*/*.psd1 | Select-Object -ExpandProperty BaseName
    $ModuleSrcPath = Resolve-Path "./src/${ModuleName}/"
    $ProjectPath = Resolve-Path "${ModuleSrcPath}/${ModuleName}.fsproj"
    $ModuleVersion = ($ProjectPath | Select-Xml '//Version/text()').Node.Value
    $TargetFrameworks = ($ProjectPath | Select-Xml '//TargetFrameworks/text()').Node.Value -split ';'
    $ModulePublishPath = Resolve-Path "./publish/${ModuleName}/"
    $ModuleManifestPath = "${ModulePublishPath}/${ModuleName}.psd1"
    $TestResultsRootPath = "./src/${ModuleName}.Test/TestResults/"
    "Module: ${ModuleName} ver${ModuleVersion} root=${ModuleSrcPath} publish=${ModulePublishPath}"
}

Task default -Depends TestAll

# NOTE: I don't know why, but if I add Lint before E2ETest, the module import will be doubled.
Task TestAll -Depends Init, Build, UnitTest, E2ETest, Lint

Task Init {
    'Init is running!'
    dotnet tool restore
}

Task Clean {
    'Clean is running!'
    Get-Module $ModuleName -All | Remove-Module -Force -ErrorAction SilentlyContinue
    @(
        "./src/*/*/${Stage}"
        "${ModulePublishPath}/*"
    ) | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue -Exclude .gitignore, .gitkeep
}

function Get-ValidMarkdownCommentHelp {
    if (Get-Command Measure-PlatyPSMarkdown -ErrorAction SilentlyContinue) {
        $help = Measure-PlatyPSMarkdown "./docs/${ModuleName}/*.md" | Where-Object Filetype -Match CommandHelp
        $validations = $help.FilePath | Test-MarkdownCommandHelp -DetailView
        if (-not $validations.IsValid) {
            $validations.Messages | Where-Object { $_ -notlike 'PASS:*' } | Write-Error
            throw 'Invalid markdown help files.'
        }
        $help
    }
    else {
        Write-Warning 'PlatyPS is not installed.'
    }
}

Task Lint {
    # F# analysis
    dotnet fantomas ./src --check --verbosity detailed
    if (-not $?) {
        throw 'dotnet fantomas failed.'
    }
    $analyzerPath = dotnet build $ModuleSrcPath -f 'netstandard2.0' --getProperty:PkgIonide_Analyzers
    Get-ChildItem './src/*/*.fsproj' | ForEach-Object {
        dotnet fsharp-analyzers --project $_ --analyzers-path $analyzerPath --report "analysis/$($_.BaseName)-report.sarif" --code-root src --exclude-files '**/obj/**/*' '**/bin/**/*'
        if (-not $?) {
            throw "dotnet fsharp-analyzers for $($_.BaseName) failed."
        }
    }

    # PowerShell analysis
    @('./psakefile.ps1', "./tests/${ModuleName}.Tests.ps1") | ForEach-Object {
        $warn = Invoke-ScriptAnalyzer -Path $_ -Settings ./PSScriptAnalyzerSettings.psd1
        if ($warn) {
            $warn
            throw "Invoke-ScriptAnalyzer for ${_} failed."
        }
    }
    Get-ValidMarkdownCommentHelp | Out-Null
}

function Get-FullModuleVersion {
    param (
        [Parameter(Mandatory, Position = 0, ValueFromPipeline)]
        [ValidateNotNull()]
        [psobject]
        $Module
    )
    # NOTE: Short hand operator is not supported in PowerShell 5.1.
    "$(if ($Module.ModuleVersion) { $Module.ModuleVersion } else { $Module.Version })$(if ($Module.PrivateData.PSData.Prerelease) { "-$($Module.PrivateData.PSData.Prerelease)" } else { '' })"
}

Task Build -Depends Clean {
    'Build command let!'
    $module = Import-PowerShellDataFile "${ModuleSrcPath}/${ModuleName}.psd1"
    $ManifestModuleVersion = $module | Get-FullModuleVersion
    if ($ManifestModuleVersion -ne $ModuleVersion) {
        throw "Version inconsistency between Module manifest (.psd1) and project (.fsproj). .psd1: ${ManifestModuleVersion}, .fsproj: ${ModuleVersion}"
    }
    dotnet build -c $Stage
    if (-not $?) {
        throw 'dotnet build failed.'
    }
    $TargetFrameworks | ForEach-Object {
        "Build ${ModuleName} ver${ModuleVersion} for target framework: ${_}"
        dotnet publish -c $Stage -f $_ $ModuleSrcPath
        if (-not $?) {
            throw 'dotnet publish failed.'
        }
    }
    "Completed to build ${ModuleName} ver${ModuleVersion}"
}

Task UnitTest {
    Remove-Item "${TestResultsRootPath}/*" -Recurse -ErrorAction SilentlyContinue
    $TargetFrameworks | ForEach-Object {
        "Run unit tests for target framework: ${_}"
        dotnet test -p:TestTargetFramework=$_ --collect:"XPlat Code Coverage" --nologo --logger:"console;verbosity=detailed" --blame-hang-timeout 15s --blame-hang-dump-type full
        if (-not $?) {
            throw "dotnet test failed for target framework: ${_}"
        }
        Move-Item "${TestResultsRootPath}/*/coverage.cobertura.xml" "${TestResultsRootPath}/coverage.${_}.cobertura.xml" -Force
    }
}

Task Coverage -Depends UnitTest {
    Remove-Item ./coverage/*
    reportgenerator -reports:"${TestResultsRootPath}/coverage.*.cobertura.xml" -targetdir:'coverage' -reporttypes:Html
}

Task WorkflowTest {
    if (-not (Get-Command act -ErrorAction SilentlyContinue)) {
        throw 'act is not installed. Read https://github.com/nektos/act and install it.'
    }
    act pull_request --verbose --platform ubuntu-latest=catthehacker/ubuntu:pwsh-latest
}

Task Benchmark {
    # NOTE: ex) invoke-psake -taskList Benchmark -parameters @{Filter='*invokeAction*';TestTargetFramework='net6.0'}
    dotnet run --project ./src/pocof.Benchmark -c Release --filter $Filter -p:TestTargetFramework=$TestTargetFramework
}

Task MemoryLayout {
    # NOTE: ex) Invoke-psake -taskList MemoryLayout -parameters @{'Group'='Pocof';}
    dotnet run --project ./src/pocof.Inspector $Group
}

Task Import -Depends Build {
    "Import ${ModuleName} ver${ModuleVersion}"
    if ( -not ($ModuleName -and $ModuleVersion)) {
        throw "ModuleName or ModuleVersion not defined. ${ModuleName}, ${ModuleVersion}"
    }
    $module = Get-ChildItem $ModuleManifestPath
    if (-not $module) {
        throw "Module manifest not found. $($module.ModuleBase)/*.psd1"
    }
    Test-ModuleManifest -Path $module -ErrorAction Stop
    $module | Import-Module -Global -Verbose
}

Task E2ETest -Depends Import {
    $result = Invoke-Pester -PassThru
    if ($result.Failed) {
        throw 'Invoke-Pester failed.'
    }
}

Task ExternalHelp {
    $help = Get-ValidMarkdownCommentHelp
    # NOTE: ex) Invoke-psake -taskList ExternalHelp -parameters @{'SkipUpdateMarkdown'=$true;}
    # NOTE: Regenerating markdown command help sometimes causes unintended modifications.
    if (-not $SkipUpdateMarkdown) {
        $help.FilePath | Update-MarkdownCommandHelp -NoBackup
    }
    $help.FilePath | Import-MarkdownCommandHelp | Export-MamlCommandHelp -OutputFolder ./src/ -Force | Out-Null
}

Task Release -PreCondition { $Stage -eq 'Release' } -Depends TestAll {
    "Release ${ModuleName}! version=${ModuleVersion} dryrun=${DryRun}"

    $module = Import-PowerShellDataFile $ModuleManifestPath
    $ManifestModuleVersion = $module | Get-FullModuleVersion
    if ($ManifestModuleVersion -ne $ModuleVersion) {
        throw "Version inconsistency between Module manifest (.psd1) and project (.fsproj). .psd1: ${ManifestModuleVersion}, .fsproj: ${ModuleVersion}"
    }

    $Params = @{
        Path = $ModulePublishPath
        Repository = 'PSGallery'
        ApiKey = (Get-Credential API-key -Message 'Enter your API key as the password').GetNetworkCredential().Password
        WhatIf = $DryRun
        Verbose = $true
    }
    Publish-PSResource @Params
}
