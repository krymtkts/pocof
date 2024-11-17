Describe 'pocof' {
    Context 'pocof module' {
        It 'Given the pocof module, it should have a nonzero version' {
            $m = Get-Module pocof
            $m.Version | Should -Not -Be $null
            # NOTE: to allow 0 major version.
            # if (-not $m.PrivateData.PSData.Prerelease) {
            #     $m.Version.Major | Should -BeGreaterThan 0
            # }
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
                @{ BeExactly = $true; ExpectedValue = $Expected }
            }
            else {
                @{ BeNullOrEmpty = $true }
            }
            $_ | Should @p
        }
    }

    Context 'Checking Select-Pocof parameters' -ForEach @{
        InputObject = 'Hello,world'
        Expected = 'Hello,world'
        BaseParam = @{NonInteractive = $true };
    } {
        Context 'In <Layout> mode with empty query' -ForEach @(
            @{ Layout = 'TopDown' },
            @{ Layout = 'TopDownHalf' },
            @{ Layout = 'BottomUp' }
            @{ Layout = 'BottomUpHalf' }
        ) {
            It "Given Layout '<Layout>', '<Expected>' should be returned." -TestCases @(
                @{ Params = $BaseParam + $_ }
            ) {
                $InputObject | Select-Pocof @Params -ErrorAction SilentlyContinue | Should -BeExactly -ExpectedValue $Expected
            }
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
                @{ Expected = 'Hello,world'; Params = $BaseParam + $_ }
                @{ Expected = 'Hello,world'; Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{ Expected = 'Hello,world'; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
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
                @{ Expected = 'Hello,world' ; Params = $BaseParam + $_ }
                @{ Expected = $null; Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{ Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) $PocofShould
        }
        Context 'In <Matcher> mode with composite query "and" (default)' -ForEach @(
            @{ Matcher = 'MATCH'; Query = 'hello World' },
            @{ Matcher = 'LIKE'; Query = '*d hell*' },
            @{ Matcher = 'EQ'; Query = 'hello,world Hello,world' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{ Expected = 'Hello,world' ; Params = $BaseParam + $_ }
                @{ Expected = $null; Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{ Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) $PocofShould
        }
        Context 'In <Matcher> mode with composite query "or"' -ForEach @(
            @{ Matcher = 'MATCH'; Query = 'universe hello' ; Operator = 'or' }; ,
            @{ Matcher = 'LIKE'; Query = 'd* hell*' ; Operator = 'or' }; ,
            @{ Matcher = 'EQ'; Query = 'hello,world hello,universe' ; Operator = 'or' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{ Expected = 'Hello,world' ; Params = $BaseParam + $_ }
                @{ Expected = 'Hello,world'; Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{ Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) $PocofShould
        }
        It 'Given PSCustomObject, should be returned without any mutation.' -TestCases @(
            @{ InputObject = @([PSCustomObject]@{Name = 'McCall' }); Params = $BaseParam }
        ) {
            $InputObject | Select-Pocof @Params | Should -BeExactly -ExpectedValue $InputObject
        }
        It "Given '<InputObject>', it can be filtered to '<Expected>' with `Select-Object -First 1`." -TestCases @(
            @{ InputObject = 1..1; Expected = 1 ; Params = $BaseParam }
            @{ InputObject = 1..100; Expected = 1; Params = $BaseParam }
        ) {
            $InputObject | Select-Pocof @Params | Select-Object -First 1 | ForEach-Object {
                $_ | Should -BeExactly -ExpectedValue $Expected
            }
        }
        Context 'for Hashtable' -ForEach @(
            @{ Operator = 'or' }
        ) {
            It "Given '<InputObject>', it keeps order as '<Expected>'." -TestCases @(
                @{ InputObject = [ordered]@{a = 1; b = 2; c = 3 }; Expected = @(
                        [Collections.DictionaryEntry]::new('a', 1),
                        [Collections.DictionaryEntry]::new('b', 2),
                        [Collections.DictionaryEntry]::new('c', 3)) ; Params = $BaseParam + $_
                }
                @{ InputObject = 1..20; Expected = 1..20 ; Params = $BaseParam }
            ) {
                $InputObject | Select-Pocof @Params | Should -BeExactly -ExpectedValue $Expected
                $tmp = @{ InputObject = $InputObject } + $Params
                Select-Pocof @tmp | Should -BeExactly -ExpectedValue $Expected
            }
            It "Given '<InputObject>', it can be filtered to '<Expected>'." -TestCases @(
                @{ InputObject = [ordered]@{a = 1; b = 2; c = 3; d = 'a1' }; Expected = @(
                        [Collections.DictionaryEntry]::new('a', 1),
                        [Collections.DictionaryEntry]::new('d', 'a1')) ; Params = $BaseParam + $_ + @{Query = '1' }
                }
                @{ InputObject = [ordered]@{a = 1; b = 2; c = 3; d = 'a1' }; Expected = @(
                        [Collections.DictionaryEntry]::new('d', 'a1')) ; Params = $BaseParam + $_ + @{Query = 'a1' }
                }
                @{ InputObject = [ordered]@{a = 1; b = 2; c = 3; d = 'a1' }; Expected = @(
                        [Collections.DictionaryEntry]::new('a', 1)) ; Params = $BaseParam + $_ + @{Query = ':key a' }
                }
                @{ InputObject = [ordered]@{}; Expected = @() ; Params = $BaseParam + $_ }
            ) {
                $InputObject | Select-Pocof @Params | Should -BeExactly -ExpectedValue $Expected
                $tmp = @{ InputObject = $InputObject } + $Params
                # for testing direct passing to InputObject.
                Select-Pocof @tmp | Should -BeExactly -ExpectedValue $Expected
            }
        }

        Context 'with culture' -ForEach @(
            @{ Matcher = 'match'; Operator = 'or'; Query = '24-01' }
        ) {
            BeforeEach {
                $global:culture = [System.Threading.Thread]::CurrentThread.CurrentCulture
                $global:testCulture = [System.Globalization.CultureInfo]::GetCultureInfo('en-US').Clone()
                $global:testCulture.DateTimeFormat.ShortDatePattern = 'yyyy-MM-dd'
                [System.Threading.Thread]::CurrentThread.CurrentCulture = $global:testCulture
            }
            AfterEach {
                [System.Threading.Thread]::CurrentThread.CurrentCulture = $global:culture
            }
            It "Given '<InputObject>', it keeps order as '<Expected>'." -TestCases @(
                @{ InputObject = 1..12 | ForEach-Object {
                        Get-Date ('2024-{0:D2}-01' -f $_)
                    }; Expected = @(
                        Get-Date '2024-01-01'
                    ) ; Params = $BaseParam + $_
                }
            ) {
                $InputObject | Select-Pocof @Params | Should -BeExactly -ExpectedValue $Expected
            }
        }

        Context 'with unique switch' -ForEach @(
            @{ Unique = $true }
        ) {
            It "Given '<InputObject>', it exclude duplications as '<Expected>'." -TestCases @(
                @{ InputObject = (1..10) + (1..20); Expected = 1..20 ; Params = $BaseParam + $_ }
                @{ InputObject = @(
                        [ordered]@{a = 1; b = 2; c = 3 }
                        [ordered]@{b = 2; c = 3; d = 4 }
                    )
                    Expected = @(
                        [Collections.DictionaryEntry]::new('a', 1)
                        [Collections.DictionaryEntry]::new('b', 2)
                        [Collections.DictionaryEntry]::new('c', 3)
                        [Collections.DictionaryEntry]::new('d', 4)
                    )
                    Params = $BaseParam + $_
                }
            ) {
                $InputObject | Select-Pocof @Params | Should -BeExactly -ExpectedValue $Expected
                $tmp = @{ InputObject = $InputObject } + $Params
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
                @{ Expected = @($InputObject[0]); Params = $BaseParam + $_ }
                @{ Expected = @($InputObject[1]); Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{ Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) $PocofShould
        }

        Context 'In <Matcher> mode with composite query "and" (default)' -ForEach @(
            @{ Matcher = 'MATCH'; Query = ':name  os tok' },
            @{ Matcher = 'MATCH'; Query = ':city tok  :name os' },
            @{ Matcher = 'LIKE'; Query = ':Name  os* tok*' },
            @{ Matcher = 'LIKE'; Query = ':City *yo  :Name *sk*' },
            @{ Matcher = 'EQ'; Query = ':namE  osaka tokyo' }
            @{ Matcher = 'EQ'; Query = ':city tokyo  :namE osaka' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{ Expected = @($InputObject[0]); Params = $BaseParam + $_ }
                @{ Expected = @($InputObject[1]); Params = @{InvertQuery = $true } + $BaseParam + $_ }
                @{ Expected = $null; Params = @{CaseSensitive = $true } + $BaseParam + $_ }
            ) $PocofShould
        }
    }
    Context 'Select-Pocof cmdlet with huge records' -ForEach @{
        InputObject = @(1..100000); BaseParam = @{NonInteractive = $true };
    } {
        Context 'In <Matcher> mode with single property "and" (default)' -ForEach @(
            @{ Matcher = 'MATCH'; Query = '99999' },
            @{ Matcher = 'LIKE'; Query = '99999' },
            @{ Matcher = 'EQ'; Query = '99999' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{ Expected = @(99999); Params = $BaseParam + $_ }
            ) $PocofShould
        }
    }
    Context 'Select-Pocof cmdlet with huge hashmap' -ForEach @{
        InputObject = @(1..100000) | ForEach-Object -Begin { $ret = @{} } -Process { $ret[$_] = $_ } -End { $ret }
        BaseParam = @{NonInteractive = $true }
    } {
        Context 'In <Matcher> mode with single property "and" (default)' -ForEach @(
            @{ Matcher = 'MATCH'; Query = ':key 99999' },
            @{ Matcher = 'LIKE'; Query = ':key 99999' },
            @{ Matcher = 'EQ'; Query = ':key 99999' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{ Expected = @([Collections.DictionaryEntry]::new(99999, 99999)); Params = $BaseParam + $_ }
            ) $PocofShould
        }
    }
    Context 'Select-Pocof cmdlet with huge PSObject' -ForEach @{
        InputObject = @(1..100000) | ForEach-Object { [PSCustomObject]@{ Name = $_ } }
        BaseParam = @{NonInteractive = $true }
    } {
        Context 'In <Matcher> mode with single property "and" (default)' -ForEach @(
            @{ Matcher = 'MATCH'; Query = ':name 99999' },
            @{ Matcher = 'LIKE'; Query = ':name 99999' },
            @{ Matcher = 'EQ'; Query = ':name 99999' }
        ) {
            It "Given a '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{ Expected = @($InputObject[-2]); Params = $BaseParam + $_ }
            ) $PocofShould
        }
    }

    Context 'Checking Select-Pocof options' {
        $PocofShouldPass = {
            It "Should pass the check for <Name>=`$null" -TestCases @(
                @{ Params = @{ NonInteractive = $true; $Name = $null } }
            ) {
                { Select-Pocof @Params } | Should @Expected
            }
        }
        $NullValidationMessage = "Cannot validate argument on parameter '*'. The argument is null"
        $ExpectedMessageForStringParameter = "$NullValidationMessage. Provide a valid value for the argument, and then try running the command again."
        $ExpectedMessageForNonEmptyStringParameter = "$NullValidationMessage or empty. Provide an argument that is not null or empty, and then try the command again."
        $ExpectedMessageForValidateSetParameter = "$NullValidationMessage, empty, or an element of the argument collection contains a null value. Supply a collection that does not contain any null values and then try the command again."
        $ExpectedMessageForSwitchParameter = '*'

        Context 'For string options' -ForEach @(
            @{ Expected = @{ Throw = $true; Not = $false; ExpectedMessage = $ExpectedMessageForStringParameter } }
        ) {
            Context '<Name>' -ForEach @(
                @{ Name = 'InputObject' }
                @{ Name = 'Query' }
                @{ Name = 'Prompt' }
            ) $PocofShouldPass
        }

        Context 'For non-empty string options' -ForEach @(
            @{ Name = 'WordDelimiters'; Expected = @{ Throw = $true; Not = $false; ExpectedMessage = $ExpectedMessageForNonEmptyStringParameter } }
        ) $PocofShouldPass

        Context 'For validate set options' -ForEach @(
            @{ Expected = @{ Throw = $true; Not = $false; ExpectedMessage = $ExpectedMessageForValidateSetParameter } }
        ) {
            Context '<Name>' -ForEach @(
                @{ Name = 'Matcher' }
                @{ Name = 'Operator' }
                @{ Name = 'Layout' }
            ) $PocofShouldPass
        }

        Context 'For switch options' -ForEach @(
            @{ Expected = @{ Throw = $true; Not = $true; ExpectedMessage = $ExpectedMessageForSwitchParameter } }
        ) {
            Context '<Name>' -ForEach @(
                @{ Name = 'CaseSensitive' }
                @{ Name = 'InvertQuery' }
                @{ Name = 'SuppressProperties' }
                @{ Name = 'Unique' }
            ) $PocofShouldPass
        }

        Context 'For NonInteractive options' {
            It "Shouldn't fail with <Name>=`$null" -TestCases @(
                @{ Params = @{ NonInteractive = $null }; Name = 'NonInteractive' }
            ) {
                { Select-Pocof @Params } | Should -Not -Throw $ExpectedMessageForSwitchParameter
            }
        }
    }
}
