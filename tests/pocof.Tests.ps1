Describe 'pocof' {
    Context 'pocof module' {
        It 'Given the pocof module, it should have a nonzero version' {
            $m = Get-Module pocof
            $m.Version | Should -Not -Be $null
            $m.Version.Major | Should -BeGreaterThan 0
        }
        It 'Given the pocof module, it should have <Expected> commands' -TestCases @(
            @{ Expected = 'Select-Pocof' }
        ) {
            Param($InputObject)
            $m = Get-Module pocof
            ($m.ExportedCmdlets).Values | Select-Object -ExpandProperty Name | Should -Contain $Expected
        }
    }
    Context 'Select-Pocof cmdlet' {
        It "Given a name '<Name>', '<Expected>' should be returned." -TestCases @(
            @{ Name = 'Hello, world'; Expected = 'Hello, world' }
        ) {
            Param($InputObject)
            Select-Pocof $Name -NonInteractive | Should -BeExactly $Expected
        }
    }
}