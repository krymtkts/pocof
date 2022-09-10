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
        InputObject = 'Hello, world'; BaseParam = @{NonInteractive = $true };
    } {
        Context ' In <Matcher> mode' -ForEach @(
            @{ Matcher = 'MATCH'; Query = 'hello' },
            @{ Matcher = 'LIKE'; Query = 'hello*' },
            @{ Matcher = 'EQ'; Query = 'hello, world' }
        ) {
            It " Given a name '<InputObject>', '<Expected>' should be returned." -TestCases @(
                @{Expected = 'Hello, world' ; Params = $BaseParam + $_ }
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
    }
}