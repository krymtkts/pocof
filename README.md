# Pocof

[![PowerShell Gallery](https://img.shields.io/powershellgallery/v/pocof)](https://www.powershellgallery.com/packages/pocof)
[![PowerShell Gallery](https://img.shields.io/powershellgallery/dt/pocof)](https://www.powershellgallery.com/packages/pocof)
![Test main status](https://github.com/krymtkts/pocof/actions/workflows/main.yml/badge.svg)
[![codecov](https://codecov.io/gh/krymtkts/pocof/graph/badge.svg?token=7HA9NC8PHT)](https://codecov.io/gh/krymtkts/pocof)

An interactive pipeline filtering Cmdlet for PowerShell written in F#.
Pocof implemented in .Net Standard 2.0, ensuring compatibility across Windows, Linux, and Mac.

A fork of [poco](https://github.com/jasonmarcher/poco) by jasonmarcher.
poco based on [powershell peco](https://gist.github.com/yumura/8df37c22ae1b7942dec7) by yumura.
powershell peco is a port of [peco](https://github.com/peco/peco) for PowerShell.

## Installation

[PowerShell Gallery | pocof](https://www.powershellgallery.com/packages/pocof/)

```powershell
# PowerShellGet 2.x
Install-Module -Name pocof -AllowPrerelease

# PSResourceGet (also known as PowerShellGet 3.0)
Install-PSResource -Name pocof -Prerelease
```

## Motivation

To learn how to write Cmdlet in F#.
Also to add my own flavor to poco which I often use.

## Cmdlet help

See [`Select-Pocof.md`](./docs/pocof/Select-Pocof.md)

## Disclaimer

- Some features are not implemented yet. I have created issues for these planned features.
- Some objects cannot filter like PowerShell.
- I mainly use Windows. I tested pocof in Linux on Ubuntu on WSL. I have not tested Mac as I do not have a Mac machine. If you encounter any bugs depends on Linux or Mac, please create an issue with details.
- The help for some parameters output raw Markdown because PlatyPS cannot render table and bullet list to MAML.
- PowerShell ISE is not supported.

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
