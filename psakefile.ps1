Properties {
    if (-not $Stage) {
        $Stage = 'Debug'
    }
    if (-not $Mode) {
        $Mode = 'DryRun'
    }
    $ModuleName = Resolve-Path ./src/* | Split-Path -Leaf
    $ModuleVersion = (Resolve-Path ./src/*/*.fsproj | Select-Xml '//Version/text()').Node.Value
    $ModuleRoot = Split-Path -Parent $PSCommandPath
    "Module: $ModuleName ver$ModuleVersion root=$ModuleRoot"
}

Task default -depends TestAll

Task TestAll -depends Init, Build, Test

Task Init {
    'Init is running!'
}

Task Clean {
    'Clean is running!'
    Get-Module pocof -All | Remove-Module -Force -ErrorAction SilentlyContinue
    Remove-Item .\src\*\bin -Recurse -Force -ErrorAction SilentlyContinue
    Remove-Item .\release -Recurse -Force -ErrorAction SilentlyContinue
}

Task Build -depends Clean {
    'Build command let!'
    Import-LocalizedData -BindingVariable module -BaseDirectory (Resolve-Path ./src/*) -FileName "$ModuleName.psd1"
    if ($module.ModuleVersion -ne (Resolve-Path ./src/*/*.fsproj | Select-Xml '//Version/text()').Node.Value) {
        throw 'Module manifest (.psd1) version does not match project (.fsproj) version.'
    }
    dotnet publish -c $Stage
    "Completed to build $ModuleName ver$ModuleVersion"
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
            $installPath = Join-Path $ModuleRoot release -AdditionalChildPath $ModuleVersion
            Copy-Item (Resolve-Path ./src/*/bin/Release/*/publish/*) $installPath -Verbose -Force
            Import-Module (Resolve-Path $installPath/*.psd1) -Global
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
    New-ExternalHelp docs -OutputPath (Resolve-Path ./src/*/) -Force
}

Task Release -precondition { $Stage -eq 'Release' } -depends Test, ExternalHelp {
    'Release It!'

    $Params = @{
        Path        = ./release/$ModuleVersion
        NugetAPIKey = (Get-Credential API-key -Message 'Enter your API key as the password').Password
        Verbose     = $true
        WhatIf      = $Mode -eq 'DryRun'
    }
    Publish-Module @Params
}