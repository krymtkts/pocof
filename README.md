# Pocof

[![PowerShell Gallery](https://img.shields.io/powershellgallery/dt/pocof?style=flat-square)](https://www.powershellgallery.com/packages/pocof)

An interactive pipeline filtering Cmdlet for PowerShell written in F#.

A fork of [poco](https://github.com/jasonmarcher/poco) by jasonmarcher.
poco based on [powershell peco](https://gist.github.com/yumura/8df37c22ae1b7942dec7) by yumura.
powershell peco is a port of [peco](https://github.com/peco/peco) for PowerShell.

## Motivation

To learn how to write Cmdlet in F#.
Also to add my own flavor to poco which I often use.

## Disclaimer

Still incomplete.

## Publishing module

Publishing to PowerShell Gallery.
To avoid accidents, dry runs mode is default.

```powershell
# DryRun.
Invoke-psake -taskList Release -parameters @{'Stage'='Release'}

# Publish.
Invoke-psake -taskList Release -parameters @{'Stage'='Release';'DryRun'=$false}
```
