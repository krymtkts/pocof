Task default -depends TestAll

Task TestAll -depends Init, Clean, Build, Test

Task Init {
    'Init is running!'
}

Task Clean {
    'Clean is running!'
    Remove-Module pocof -Force -ErrorAction SilentlyContinue
    Remove-Item .\src\*\bin -Recurse -Force
}

Task Build {
    'Build command let!'
    $ModuleName = Resolve-Path ./src/* | Split-Path -Leaf
    Import-LocalizedData -BindingVariable module -BaseDirectory (Resolve-Path ./src/*) -FileName "$ModuleName.psd1"
    if ($module.ModuleVersion -ne (Resolve-Path ./src/*/*.fsproj | Select-Xml '//Version/text()').Node.Value) {
        throw 'Module manifest (.psd1) version does not match project (.fsproj) version.'
    }
    dotnet publish
}

Task Test {
    Import-Module (Resolve-Path ./src/*/bin/Debug/*/publish/*.psd1) -Force
    Invoke-Pester
}
