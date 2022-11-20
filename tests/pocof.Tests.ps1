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
            ) {
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
        }
        Context 'In <Matcher> mode with composite query "or"' -ForEach @(
            @{ Matcher = 'MATCH'; Query = 'universe hello' },
            @{ Matcher = 'LIKE'; Query = 'd* hell*' },
            @{ Matcher = 'EQ'; Query = 'hello,world hello,universe' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{Expected = 'Hello,world' ; Params = $BaseParam + $_ }
                @{Expected = 'Hello,world'; Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) {
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
        }
        It "Given '<InputObject>', it can be filtered to '<Expected>' with `Select-Object -First 1`." -TestCases @(
            @{InputObject = 1..1; Expected = 1 ; Params = $BaseParam }
            @{InputObject = 1..100; Expected = 1; Params = $BaseParam }
        ) {
            $InputObject | Select-Pocof @Params | Select-Object -First 1 | ForEach-Object {
                $_ | Should -BeExactly -ExpectedValue $Expected
            }
        }
        It "Given '<InputObject>', it keeps order as '<Expected>'." -TestCases @(
            @{InputObject = [ordered]@{a = 1; b = 2; c = 3 }; Expected = @(
                    [Collections.DictionaryEntry]::new('a', 1),
                    [Collections.DictionaryEntry]::new('b', 2),
                    [Collections.DictionaryEntry]::new('c', 3)) ; Params = $BaseParam
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
                    [Collections.DictionaryEntry]::new('d', 'a1')) ; Params = $BaseParam + @{Query = '1' }
            }
            @{InputObject = [ordered]@{a = 1; b = 2; c = 3; d = 'a1' }; Expected = @(
                    [Collections.DictionaryEntry]::new('d', 'a1')) ; Params = $BaseParam + @{Query = 'a1' }
            }
        ) {
            $InputObject | Select-Pocof @Params | Should -BeExactly -ExpectedValue $Expected
            $tmp = @{InputObject = $InputObject } + $Params
            Select-Pocof @tmp | Should -BeExactly -ExpectedValue $Expected
        }
    }
}