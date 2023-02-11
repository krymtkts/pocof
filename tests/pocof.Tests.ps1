Describe 'pocof' {
    Context 'pocof module' {
        It 'Given the pocof module, it should have a nonzero version' {
            $m = Get-Module pocof
            $m.Version | Should -Not -Be $null
            if (-not $m.PrivateData.PSData.Prerelease) {
                $m.Version.Major | Should -BeGreaterThan 0
            }
        }
        It 'Given the pocof module, it should have <Expected> commands' -TestCases @(
            @{ Expected = 'Select-Pocof' }
        ) {
            $m = Get-Module pocof
            ($m.ExportedCmdlets).Values | Select-Object -ExpandProperty Name | Should -Contain $Expected
        }
    }
    $PocofShould = {
        $InputObject | Select-Pocof @Params | ForEach-Object {
            $p = if ($Expected) {
                @{BeExactly = $true; ExpectedValue = $Expected }
            }
            else {
                @{BeNullOrEmpty = $true }
            }
            $_ | Should @p
        }
    }
    Context 'Select-Pocof cmdlet' -ForEach @{
        InputObject = 'Hello,world'; BaseParam = @{NonInteractive = $true };
    } {
        Context 'In <Matcher> mode with empty query' -ForEach @(
            @{ Matcher = 'MATCH' },
            @{ Matcher = 'LIKE' },
            @{ Matcher = 'EQ' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{Expected = 'Hello,world' ; Params = @{Query = '' } + $BaseParam + $_ }
            ) {
                $InputObject | Select-Pocof @Params | Should -BeExactly -ExpectedValue $Expected
            }
        }
        Context 'In <Matcher> mode single query' -ForEach @(
            @{ Matcher = 'MATCH'; Query = 'hello' },
            @{ Matcher = 'LIKE'; Query = 'hello*' },
            @{ Matcher = 'EQ'; Query = 'hello,world' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{Expected = 'Hello,world' ; Params = $BaseParam + $_ }
                @{Expected = $null; Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) $PocofShould
        }
        Context 'In <Matcher> mode with composite query "and" (default)' -ForEach @(
            @{ Matcher = 'MATCH'; Query = 'hello World' },
            @{ Matcher = 'LIKE'; Query = '*d hell*' },
            @{ Matcher = 'EQ'; Query = 'hello,world Hello,world' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{Expected = 'Hello,world' ; Params = $BaseParam + $_ }
                @{Expected = $null; Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) $PocofShould
        }
        Context 'In <Matcher> mode with composite query "or"' -ForEach @(
            @{ Matcher = 'MATCH'; Query = 'universe hello' ; Operator = 'or' }; ,
            @{ Matcher = 'LIKE'; Query = 'd* hell*' ; Operator = 'or' }; ,
            @{ Matcher = 'EQ'; Query = 'hello,world hello,universe' ; Operator = 'or' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{Expected = 'Hello,world' ; Params = $BaseParam + $_ }
                @{Expected = 'Hello,world'; Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) $PocofShould
        }
        Context 'In <Matcher> mode with composite query "none"' -ForEach @(
            @{ Matcher = 'MATCH'; Query = 'universe hello' ; Operator = 'none' }; ,
            @{ Matcher = 'LIKE'; Query = 'd* hell*' ; Operator = 'none' }; ,
            @{ Matcher = 'EQ'; Query = 'hello,world hello,universe' ; Operator = 'none' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{Expected = $null ; Params = $BaseParam + $_ }
            ) $PocofShould
        }
        It 'Given PSCustomObject, should be returned without any mutation.' -TestCases @(
            @{InputObject = @([PSCustomObject]@{Name = 'McCall' }); Params = $BaseParam }
        ) {
            $InputObject | Select-Pocof @Params | Should -BeExactly -ExpectedValue $InputObject
        }
        It "Given '<InputObject>', it can be filtered to '<Expected>' with `Select-Object -First 1`." -TestCases @(
            @{InputObject = 1..1; Expected = 1 ; Params = $BaseParam }
            @{InputObject = 1..100; Expected = 1; Params = $BaseParam }
        ) {
            $InputObject | Select-Pocof @Params | Select-Object -First 1 | ForEach-Object {
                $_ | Should -BeExactly -ExpectedValue $Expected
            }
        }
        Context 'for Hashtable' -ForEach @(
            @{ Operator = 'or' }
        ) {
            It "Given '<InputObject>', it keeps order as '<Expected>'." -TestCases @(
                @{InputObject = [ordered]@{a = 1; b = 2; c = 3 }; Expected = @(
                        [Collections.DictionaryEntry]::new('a', 1),
                        [Collections.DictionaryEntry]::new('b', 2),
                        [Collections.DictionaryEntry]::new('c', 3)) ; Params = $BaseParam + $_
                }
                @{InputObject = 1..20; Expected = 1..20 ; Params = $BaseParam }
            ) {
                $InputObject | Select-Pocof @Params | Should -BeExactly -ExpectedValue $Expected
                $tmp = @{InputObject = $InputObject } + $Params
                Select-Pocof @tmp | Should -BeExactly -ExpectedValue $Expected
            }
            It "Given '<InputObject>', it can be filtered to '<Expected>'." -TestCases @(
                @{InputObject = [ordered]@{a = 1; b = 2; c = 3; d = 'a1' }; Expected = @(
                        [Collections.DictionaryEntry]::new('a', 1),
                        [Collections.DictionaryEntry]::new('d', 'a1')) ; Params = $BaseParam + $_ + @{Query = '1' }
                }
                @{InputObject = [ordered]@{a = 1; b = 2; c = 3; d = 'a1' }; Expected = @(
                        [Collections.DictionaryEntry]::new('d', 'a1')) ; Params = $BaseParam + $_ + @{Query = 'a1' }
                }
            ) {
                $InputObject | Select-Pocof @Params | Should -BeExactly -ExpectedValue $Expected
                $tmp = @{InputObject = $InputObject } + $Params
                Select-Pocof @tmp | Should -BeExactly -ExpectedValue $Expected
            }
        }
    }
    Context 'Select-Pocof cmdlet with property' -ForEach @{
        InputObject = @(
            [PSCustomObject]@{ Name = 'Osaka'; City = 'Tokyo' },
            [PSCustomObject]@{ Name = 'Tokyo'; City = 'Osaka' }
        ); BaseParam = @{NonInteractive = $true };
    } {
        Context 'In <Matcher> mode with single property "and" (default)' -ForEach @(
            @{ Matcher = 'MATCH'; Query = ':name os' },
            @{ Matcher = 'LIKE'; Query = ':Name os*' },
            @{ Matcher = 'EQ'; Query = ':namE osaka' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{Expected = @($InputObject[0]); Params = $BaseParam + $_ }
                @{Expected = @($InputObject[1]); Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) $PocofShould
        }

        Context 'In <Matcher> mode with composite query "and" (default)' -ForEach @(
            @{ Matcher = 'MATCH'; Query = ':name os tok' },
            @{ Matcher = 'MATCH'; Query = ':city tok :name os' },
            @{ Matcher = 'LIKE'; Query = ':Name os* tok*' },
            @{ Matcher = 'LIKE'; Query = ':City *yo :Name *sk*' },
            @{ Matcher = 'EQ'; Query = ':namE osaka tokyo' }
            @{ Matcher = 'EQ'; Query = ':city tokyo :namE osaka' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{Expected = @($InputObject[0]); Params = $BaseParam + $_ }
                @{Expected = @($InputObject[1]); Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) $PocofShould
        }
    }
}