---
external help file: pocof.dll-Help.xml
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

You can use 3 matching mode `match`, `like` or `eq` and can use 3 operator mode `and`, `or` or `none`.

By default, the query matches to the stringified object.
A query starting with a colon, such as `:property-name`, indicates a property query.
In a property query, you can specify the property value using the format `:property-name property-value`.
If you use a property query, the query will match to the specified object properties.

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

### Example 3: Filter hashtable

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

Select the layout: TopDown or BottomUp.
Currently, only the TopDown layout is supported.

```yaml
Type: String
Parameter Sets: (All)
Aliases:
Accepted values: TopDown, BottomUp

Required: False
Position: Named
Default value: None
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
Default value: None
Accept pipeline input: False
Accept wildcard characters: False
```

### -Query

Specifies the initial query string.

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

Select the matching mode. You can use `match`, `like`, or `eq`.

- `match` provides regular expression matching
- `like` provides wildcard matching
- `eq` provides exact matching

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
You can use `and`, `or`, or `none`.

- `and` provides logical "and" to multi query generated from splitting query by whitespace
- `or` provides logical "or" to multi query generated from splitting query by whitespace
- `none` provides simple matching with a raw query

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
