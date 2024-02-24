# Based on the `CodeFormatting.psd1` settings for PSScriptAnalyzer 1.21.0.
# https://github.com/PowerShell/PSScriptAnalyzer/blob/1.21.0/Engine/Settings/CodeFormatting.psd1
@{
    IncludeRules = @(
        'PSPlaceOpenBrace',
        'PSPlaceCloseBrace',
        'PSUseConsistentWhitespace',
        'PSUseConsistentIndentation',
        'PSAlignAssignmentStatement',
        'PSUseCorrectCasing'
    )
    Rules = @{
        PSAlignAssignmentStatement = @{
            # Disable alignment of assignment statements.
            Enable = $false
            CheckHashtable = $true
        }
        PSPlaceOpenBrace = @{
            Enable = $true
            OnSameLine = $true
            NewLineAfter = $true
            IgnoreOneLineBlock = $true
        }
        PSUseConsistentIndentation = @{
            Enable = $true
            IndentationSize = 4
            PipelineIndentation = 'IncreaseIndentationForFirstPipeline'
            Kind = 'space'
        }
        PSUseConsistentWhitespace = @{
            Enable = $true
            CheckInnerBrace = $true
            CheckOpenBrace = $true
            CheckOpenParen = $true
            CheckOperator = $true
            CheckPipe = $true
            # Check pipes.
            CheckPipeForRedundantWhitespace = $true
            CheckSeparator = $true
            CheckParameter = $false
            # Check hashtable.
            IgnoreAssignmentOperatorInsideHashTable = $false
        }
        PSUseCorrectCasing = @{
            Enable = $true
        }
    }
}
