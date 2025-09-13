---
document type: cmdlet
external help file: pocof-Help.xml
HelpUri: https://github.com/krymtkts/pocof/blob/main/docs/pocof/Select-Pocof.md
Locale: en-US
Module Name: pocof
ms.date: 02-01-2025
PlatyPS schema version: 2024-05-01
title: Select-Pocof
---

# Select-Pocof

## SYNOPSIS

Selects objects from a collection based on interactive query.

## SYNTAX

### Default (Default)

```
Select-Pocof [[-InputObject] <PSObject[]>] [-Query <String>] [-Matcher <String>]
 [-Operator <String>] [-CaseSensitive] [-InvertQuery] [-NonInteractive] [-SuppressProperties]
 [-Prompt <String>] [-Layout <String>] [-Keymaps <Hashtable>] [<CommonParameters>]
```

### \_\_AllParameterSets

```
Select-Pocof [[-Query] <string>] [-InputObject <psobject[]>] [-Matcher <string>]
 [-Operator <string>] [-CaseSensitive] [-InvertQuery] [-NonInteractive] [-SuppressProperties]
 [-Unique] [-Prompt <string>] [-Layout <string>] [-Keymaps <hashtable>] [-WordDelimiters <string>]
 [<CommonParameters>]
```

## ALIASES

## DESCRIPTION

The `Select-Pocof` cmdlet selects objects from a collection based on interactive query.
For example, you can use the `Select-Pocof` cmdlet to select files that located current directory with interactive window.

You can use 3 matching mode `Match`, `Like` or `Eq` and can use 3 operator mode `And` or `Or`.

By default, the query matches to the stringified object.
A query starting with a colon, such as `:property-name`, indicates a property query.
In a property query, you can specify the property value using the format `:property-name property-value`.
If you use a property query, the query will match to the specified object properties.

Recommend using `$PSDefaultParameterValues` if you want to use a specific option.
For example `Layout`, `Prompt` and so on.

`$PSDefaultParameterValues = @{'Select-Pocof:Layout' = 'TopDownHalf'; 'Select-Pocof:Prompt'= ''}`

## EXAMPLES

### Example 1: Get current directory items with interactive filtering

```powershell
PS C:\> Get-ChildItem | Select-Pocof
```

Interactively filter the output of `Get-ChildItem`.

### Example 2: Filter with multiple options

```powershell
PS C:\> Get-ChildItem | Select-Pocof -CaseSensitive -Query 'docs md' | Invoke-Item
```

Interactively filter the output of `Get-ChildItem` with an initial case-sensitive query.
And performs the default action on the filtered items.

### Example 3: Filter with property query

```powershell
PS C:\> Get-ChildItem | Select-Pocof -NonInteractive -Query ':Name foo'
```

Interactively filter the output of `Get-ChildItem` using a property query.
The property name in the query starts with `:` followed by a space, and then the filter value. The format is as follows.

### Example 4: Filter hashtable

```powershell
PS C:\> @{foo=100; bar=101; foobar=102} | Select-Pocof -NonInteractive -Query ':key foo' | % -Begin {$x = @{}} {$x[$_.Key]= $_.Value} -End {$x}
```

Interactively filter hashtable and create new hashtable from filtered items.

## PARAMETERS

### -CaseSensitive

Filtering in case-sensitive.

```yaml
Type: System.Management.Automation.SwitchParameter
DefaultValue: False
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues: []
HelpMessage: ""
```

### -InputObject

Specifies the objects to filter. You can also pipe the objects to `Select-Pocof`.

```yaml
Type: System.Management.Automation.PSObject[]
DefaultValue: None
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: true
    ValueFromPipelineByPropertyName: true
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues: []
HelpMessage: ""
```

### -InvertQuery

Enabling `InvertQuery` will invert the filtering output.

```yaml
Type: System.Management.Automation.SwitchParameter
DefaultValue: False
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues: []
HelpMessage: ""
```

### -Keymaps

Specifies the custom key bindings. Custom keymaps overrides default keymaps.
Default keymaps are following.

| Action                   | Keybinding                      |
| ------------------------ | ------------------------------- |
| Cancel                   | `Escape`                        |
| Cancel                   | `Ctrl` + `C`                    |
| Finish                   | `Enter`                         |
| BackwardChar             | `LeftArrow`                     |
| BackwardWord             | `Ctrl` + `LeftArrow`            |
| ForwardChar              | `RightArrow`                    |
| ForwardWord              | `Ctrl` + `RightArrow`           |
| BeginningOfLine          | `Home`                          |
| EndOfLine                | `End`                           |
| DeleteBackwardChar       | `Backspace`                     |
| DeleteForwardChar        | `Delete`                        |
| DeleteBackwardWord       | `Ctrl` + `Backspace`            |
| DeleteForwardWord        | `Ctrl` + `Delete`               |
| DeleteBackwardInput      | `Ctrl` + `Home`                 |
| DeleteForwardInput       | `Ctrl` + `End`                  |
| SelectBackwardChar       | `Shift` + `LeftArrow`           |
| SelectForwardChar        | `Shift` + `RightArrow`          |
| SelectBackwardWord       | `Ctrl` + `Shift` + `LeftArrow`  |
| SelectForwardWord        | `Ctrl` + `Shift` + `RightArrow` |
| SelectToBeginningOfLine  | `Shift` + `Home`                |
| SelectToEndOfLine        | `Shift` + `End`                 |
| SelectAll                | `Ctrl` + `A`                    |
| RotateMatcher            | `Alt` + `R`                     |
| RotateOperator           | `Alt` + `L`                     |
| ToggleCaseSensitive      | `Alt` + `C`                     |
| ToggleInvertFilter       | `Alt` + `I`                     |
| ToggleSuppressProperties | `Ctrl` + `Spacebar`             |
| CompleteProperty         | `Tab`                           |

```yaml
Type: System.Collections.Hashtable
DefaultValue: None
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues: []
HelpMessage: ""
```

### -Layout

Select the layout: TopDown, TopDownHalf, BottomUp or BottomUpHalf.

```yaml
Type: System.String
DefaultValue: TopDown
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues:
  - TopDown
  - TopDownHalf
  - BottomUp
  - BottomUpHalf
HelpMessage: ""
```

### -Matcher

Select the matching mode. You can use `Match`, `Like` or `Eq`.

- `Match` provides regular expression matching
- `Like` provides wildcard matching
- `Eq` provides exact matching

```yaml
Type: System.String
DefaultValue: Match
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues:
  - Match
  - Like
  - Eq
HelpMessage: ""
```

### -NonInteractive

Enabling `NonInteractive` starts the application in non-interactive mode.
Mainly used for testing purposes.

```yaml
Type: System.Management.Automation.SwitchParameter
DefaultValue: False
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues: []
HelpMessage: ""
```

### -Operator

Select the logical operator to use when specifying query strings.
You can use `And` or `Or`.

- `And` provides logical "And" to multi query generated from splitting query by whitespace
- `Or` provides logical "Or" to multi query generated from splitting query by whitespace

```yaml
Type: System.String
DefaultValue: And
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues:
  - And
  - Or
HelpMessage: ""
```

### -Prompt

Specifies the prompt that appears before the query input space.

```yaml
Type: System.String
DefaultValue: query
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues: []
HelpMessage: ""
```

### -Query

Specifies the initial query string.

The syntax of the query is simple.
You separate the values you want to filter with a space.

`Value1 Value2 Value3`

The property query is a combination of the property name and the filtering value.
The property name in the query starts with `:` followed by a space, and then the filter value like following.

`:PropertyName FilteringValue`

You can also combine normal filtering with property queries.

`Value1 Value2 :PropertyName Value3`

```yaml
Type: System.String
DefaultValue: None
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: 0
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues: []
HelpMessage: ""
```

### -SuppressProperties

Enabling `SuppressProperties` will disable the display of properties under the query input.

```yaml
Type: System.Management.Automation.SwitchParameter
DefaultValue: False
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues: []
HelpMessage: ""
```

### -Unique

Enabling `Unique` will exclude duplicates in `InputObject`.
`Unique` maintains the order of InputObject after excluding duplicates.
`Unique` uses the `Equals` and `GetHashCode` methods of `InputObject` to check for equality.

```yaml
Type: System.Management.Automation.SwitchParameter
DefaultValue: False
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues: []
HelpMessage: ""
```

### -WordDelimiters

Specifies the characters that delimit words for word-based cursor movement, selection or deletion functions.

```yaml
Type: System.String
DefaultValue: ;:,.[]{}()/\\|!?^&*-=+'\"–—―
SupportsWildcards: false
ParameterValue: []
Aliases: []
ParameterSets:
  - Name: (All)
    Position: Named
    IsRequired: false
    ValueFromPipeline: false
    ValueFromPipelineByPropertyName: false
    ValueFromRemainingArguments: false
DontShow: false
AcceptedValues: []
HelpMessage: ""
```

### CommonParameters

This cmdlet supports the common parameters: -Debug, -ErrorAction, -ErrorVariable,
-InformationAction, -InformationVariable, -OutBuffer, -OutVariable, -PipelineVariable,
-ProgressAction, -Verbose, -WarningAction, and -WarningVariable. For more information, see
[about_CommonParameters](https://go.microsoft.com/fwlink/?LinkID=113216).

## INPUTS

### System.Management.Automation.PSObject

## OUTPUTS

### System.Management.Automation.PSObject

## NOTES

Includes the following aliases for `Select-Pocof`

- `pocof`

## RELATED LINKS

{{ Fill in the related links here }}
