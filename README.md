# Pocof

[![PowerShell Gallery](https://img.shields.io/powershellgallery/dt/pocof)](https://www.powershellgallery.com/packages/pocof)
![Test main status](https://github.com/krymtkts/pocof/actions/workflows/main.yml/badge.svg)

An interactive pipeline filtering Cmdlet for PowerShell written in F#.

A fork of [poco](https://github.com/jasonmarcher/poco) by jasonmarcher.
poco based on [powershell peco](https://gist.github.com/yumura/8df37c22ae1b7942dec7) by yumura.
powershell peco is a port of [peco](https://github.com/peco/peco) for PowerShell.

## Motivation

To learn how to write Cmdlet in F#.
Also to add my own flavor to poco which I often use.

## Installation

[PowerShell Gallery | pocof](https://www.powershellgallery.com/packages/pocof/)

```powershell
# PowerShellGet 2.x
Install-Module -Name pocof -AllowPrerelease

# PSResourceGet (also known as PowerShellGet 3.0)
Install-PSResource -Name pocof -Prerelease
```

## Disclaimer

Still incomplete.

## Publishing module

Build and publish to PowerShell Gallery.
To avoid accidents, dry runs mode is default.

```powershell
# DryRun.
Invoke-psake -taskList Release -parameters @{'Stage'='Release'}

# Publish.
Invoke-psake -taskList Release -parameters @{'Stage'='Release';'DryRun'=$false}
```

Recommended to do these in new PowerShell process.
After you import pocof, cannot release `FSharp.Core.dll` even if you invoke `Remove-Module`.
