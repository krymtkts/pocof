---
external help file: pocof-Help.xml
Module Name: pocof
online version: https://github.com/krymtkts/pocof/blob/main/docs/Select-Pocof.md
schema: 2.0.0
---

# Select-Pocof

## SYNOPSIS

Selects objects from a collection based on interactive query.

## SYNTAX

```plaintext
Select-Pocof [[-InputObject] <PSObject[]>] [-Query <String>] [-Matcher <String>] [-Operator <String>]
 [-CaseSensitive] [-InvertQuery] [-NonInteractive] [-SuppressProperties] [-Prompt <String>] [-Layout <String>]
 [-Keymaps <Hashtable>] [<CommonParameters>]
```

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
Type: SwitchParameter
Parameter Sets: (All)
Aliases:

Required: False
Position: Named
Default value: False
Accept pipeline input: False
Accept wildcard characters: False
```

### -InputObject

Specifies the objects to filter. You can also pipe the objects to `Select-Pocof`.

```yaml
Type: PSObject[]
Parameter Sets: (All)
Aliases:

Required: False
Position: 0
Default value: None
Accept pipeline input: True (ByPropertyName, ByValue)
Accept wildcard characters: False
```

### -Keymaps

Specifies the custom key bindings. Custom keymaps overrides default keymaps.
Default keymaps are following.

```md
| Action                   | Keybinding             | Note                 |
| ------------------------ | ---------------------- | -------------------- |
| Cancel                   | `Escape`               |                      |
| Cancel                   | `Ctrl` + `C`           |                      |
| Finish                   | `Enter`                |                      |
| BackwardChar             | `LeftArrow`            |                      |
| ForwardChar              | `RightArrow`           |                      |
| BeginningOfLine          | `Home`                 |                      |
| EndOfLine                | `End`                  |                      |
| DeleteBackwardChar       | `Backspace`            |                      |
| DeleteForwardChar        | `Delete`               |                      |
| DeleteBackwardInput      | `Ctrl` + `Home`        |                      |
| DeleteForwardInput       | `Ctrl` + `End`         |                      |
| SelectBackwardChar       | `Shift` + `LeftArrow`  |                      |
| SelectForwardChar        | `Shift` + `RightArrow` |                      |
| SelectToBeginningOfLine  | `Shift` + `Home`       |                      |
| SelectToEndOfLine        | `Shift` + `End`        |                      |
| RotateMatcher            | `Alt` + `R`            |                      |
| RotateOperator           | `Alt` + `L`            |                      |
| ToggleCaseSensitive      | `Alt` + `C`            |                      |
| ToggleInvertFilter       | `Alt` + `I`            |                      |
| ToggleSuppressProperties | `Ctrl` + `Spacebar`    |                      |
| SelectLineUp             | `UpArrow`              | Not implemented yet. |
| SelectLineDown           | `DownArrow`            | Not implemented yet. |
| ScrollPageUp             | `PageUp`               | Not implemented yet. |
| ScrollPageDown           | `PageDown`             | Not implemented yet. |
| CompleteProperty         | `Tab`                  |                      |
```

```yaml
Type: Hashtable
Parameter Sets: (All)
Aliases:

Required: False
Position: Named
Default value: None
Accept pipeline input: False
Accept wildcard characters: False
```

### -Layout

Select the layout: TopDown, TopDownHalf, BottomUp or BottomUpHalf.

```yaml
Type: String
Parameter Sets: (All)
Aliases:
Accepted values: TopDown, TopDownHalf, BottomUp, BottomUpHalf

Required: False
Position: Named
Default value: TopDown
Accept pipeline input: False
Accept wildcard characters: False
```

### -Prompt

Specifies the prompt that appears before the query input space.

```yaml
Type: String
Parameter Sets: (All)
Aliases:

Required: False
Position: Named
Default value: query
Accept pipeline input: False
Accept wildcard characters: False
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
Type: String
Parameter Sets: (All)
Aliases:

Required: False
Position: Named
Default value: None
Accept pipeline input: False
Accept wildcard characters: False
```

### -InvertQuery

Enabling `InvertQuery` will invert the filtering output.

```yaml
Type: SwitchParameter
Parameter Sets: (All)
Aliases:

Required: False
Position: Named
Default value: False
Accept pipeline input: False
Accept wildcard characters: False
```

### -Matcher

Select the matching mode. You can use `Match`, `Like` or `Eq`.

- `Match` provides regular expression matching
- `Like` provides wildcard matching
- `Eq` provides exact matching

```yaml
Type: String
Parameter Sets: (All)
Aliases:
Accepted values: Match, Like, Eq

Required: False
Position: Named
Default value: Match
Accept pipeline input: False
Accept wildcard characters: False
```

### -NonInteractive

Enabling `NonInteractive` starts the application in non-interactive mode.
Mainly used for testing purposes.

```yaml
Type: SwitchParameter
Parameter Sets: (All)
Aliases:

Required: False
Position: Named
Default value: False
Accept pipeline input: False
Accept wildcard characters: False
```

### -Operator

Select the logical operator to use when specifying query strings.
You can use `And` or `Or`.

- `And` provides logical "And" to multi query generated from splitting query by whitespace
- `Or` provides logical "Or" to multi query generated from splitting query by whitespace

```yaml
Type: String
Parameter Sets: (All)
Aliases:
Accepted values: And, Or

Required: False
Position: Named
Default value: And
Accept pipeline input: False
Accept wildcard characters: False
```

### -SuppressProperties

Enabling `SuppressProperties` will disable the display of properties under the query input.

```yaml
Type: SwitchParameter
Parameter Sets: (All)
Aliases:

Required: False
Position: Named
Default value: False
Accept pipeline input: False
Accept wildcard characters: False
```

### -Unique

Enabling `Unique` will exclude duplicates in `InputObject`.
`Unique` maintains the order of InputObject after excluding duplicates.
`Unique` uses the `Equals` and `GetHashCode` methods of `InputObject` to check for equality.

```yaml
Type: SwitchParameter
Parameter Sets: (All)
Aliases:

Required: False
Position: Named
Default value: False
Accept pipeline input: False
Accept wildcard characters: False
```

### -WordDelimiters

Specifies the characters that delimit words for word-based cursor movement, selection or deletion functions.

```yaml
Type: String
Parameter Sets: (All)
Aliases:

Required: False
Position: Named
Default value: ;:,.[]{}()/\\|!?^&*-=+'\"–—―
Accept pipeline input: False
Accept wildcard characters: False
```

### CommonParameters

This cmdlet supports the common parameters: -Debug, -ErrorAction, -ErrorVariable, -InformationAction, -InformationVariable, -OutVariable, -OutBuffer, -PipelineVariable, -Verbose, -WarningAction, and -WarningVariable. For more information, see [about_CommonParameters](http://go.microsoft.com/fwlink/?LinkID=113216).

## INPUTS

### `System.Management.Automation.PSObject[]`

## OUTPUTS

### `System.Management.Automation.PSObject`

## NOTES

Includes the following aliases for `Select-Pocof`

- `pocof`

## RELATED LINKS
