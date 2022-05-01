Describe 'pocof' {
    Context 'pocof module' {
        It 'Given the pocof module, it should have a nonzero version' {
            $m = Get-Module pocof
            $m.Version | Should -Not -Be $null
            $m.Version.Major | Should -BeGreaterThan 0
        }
    }
    Context 'Select-Pocof cmdlet' {
        It "Given a name '<Name>', '<Expected>' should be returned." -TestCases @(
            @{ Name = 'Hello, world'; Expected = 'Hello, world' }
        ) {
            Param($InputObject)
            Select-Pocof $Name | Should -BeExactly $Expected
        }
    }
}