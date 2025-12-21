module PocofTest.Data

open System
open Microsoft.FSharp.Reflection
open System.Collections.Generic

open Expecto
open Expecto.Flip
open FsCheck.FSharp

open Pocof
open Pocof.Data

module LanguageExtension =
    open System.Management.Automation

    module Option =

        type Mock() =
            member val disposed = false with get, set

            interface IDisposable with
                member __.Dispose() = __.disposed <- true

    [<Tests>]
    let tests_Option =
        testList
            "Option"
            [

              test "When Some." {
                  let mock = new Option.Mock()
                  mock |> Some |> Option.dispose
                  mock.disposed |> Expect.isTrue "should call Dispose"
              }

              test "When None, shouldn't call Dispose." {
                  // NOTE: only for coverage.
                  None |> Option.dispose
              }

              ]

    module Entry =
        type MockProperty() =
            // NOTE: use a custom property inherited AdaptedProperty to call Value getter for PSObject.
            inherit PSAdaptedProperty("Dummy", "dummy")

            override __.Value
                with get () = failwith "MockProperty.Value raises error."
                and set (_) = ()

    [<Tests>]
    let tests_Entry =
        testList
            "Entry"
            [

              test "When accessing error-prone properties, shouldn't fail" {
                  // NOTE: only for coverage.
                  let a = PSObject.AsPSObject("a")
                  let p = Entry.MockProperty()
                  // NOTE: requires passing true to preValidated to skip the check for CannotAddPropertyOrMethod.
                  // https://github.com/PowerShell/PowerShell/blob/c505f4ba39111df8bd8a957f8632ff9697639f0b/src/System.Management.Automation/engine/MshMemberInfo.cs#L4598C29-L4598C30
                  a.Properties.Add(p, true)
                  a["Dummy"] |> Expect.isNull "should return None"
              }

              ]

module unwrap =
    open System.Collections
    open System.Management.Automation

    let psObjectGen =
        ArbMap.defaults
        |> ArbMap.generate<string>
        |> Gen.map (PSObject.AsPSObject >> Entry.Obj)

    let dictionaryEntryGen =
        ArbMap.defaults
        |> ArbMap.generate<string>
        |> Gen.two
        |> Gen.map (DictionaryEntry >> Entry.Dict)

    type EntryPSObject =
        static member Generate() = psObjectGen |> Arb.fromGen

    type EntryDictionaryEntry =
        static member Generate() = dictionaryEntryGen |> Arb.fromGen

    type MixedEntry =
        static member Generate() =
            Gen.oneof [ psObjectGen; dictionaryEntryGen ] |> Gen.listOf |> Arb.fromGen


    [<Tests>]
    let test_unwrap =
        testList
            "unwrap"
            [

              let config =
                  { FsCheckConfig.defaultConfig with
                      endSize = 1000
                      arbitrary = [ typeof<EntryPSObject> ] }

              testPropertyWithConfig config "When PSObject wrapped"
              <| fun data ->
                  data
                  |> unwrap
                  |> List.ofSeq
                  |> Expect.equal
                      "should return PSObject sequence"
                      (data
                       |> List.map (function
                           | Entry.Obj x -> x
                           | _ -> failwith "Dict is unreachable"))
                  |> Prop.collect (List.length data)

              let config =
                  { FsCheckConfig.defaultConfig with
                      endSize = 1000
                      arbitrary = [ typeof<EntryDictionaryEntry> ] }

              testPropertyWithConfig config "When DictionaryEntry wrapped"
              <| fun data ->
                  data
                  |> unwrap
                  |> List.ofSeq
                  |> Expect.equal
                      "should return DictionaryEntry sequence"
                      (data
                       |> List.map (function
                           | Entry.Dict x -> x
                           | _ -> failwith "Obj is unreachable"))
                  |> Prop.collect (List.length data)

              let config =
                  { FsCheckConfig.defaultConfig with
                      endSize = 1000
                      arbitrary = [ typeof<MixedEntry> ] }

              testPropertyWithConfig config "When PSObject nad DictionaryEntry wrapped"
              <| fun data ->
                  data
                  |> unwrap
                  |> List.ofSeq
                  |> Expect.equal
                      "should return mixed entry sequences"
                      (data
                       |> List.map (function
                           | Entry.Obj x -> x
                           | Entry.Dict x -> x))
                  |> Prop.collect (
                      List.length data,
                      data |> List.filter _.IsObj |> List.length,
                      data |> List.filter _.IsDict |> List.length
                  )

              ]

let randomCase (s: string) =
    let random = Random()

    s
    |> Seq.map (fun c ->
        if random.Next(2) = 0 then
            Char.ToLower(c)
        else
            Char.ToUpper(c))
    |> Seq.toArray
    |> String

let duNames<'U> = FSharpType.GetUnionCases(typeof<'U>) |> Seq.map _.Name

let randomCases (name: string) =
    [ name; name.ToLower(); name.ToUpper(); randomCase name ]

let toIgnoreCaseSet (x: string seq) =
    HashSet(x, StringComparer.InvariantCultureIgnoreCase)

let generateStringExclude (exclude: string seq) =
    let excludeSet = exclude |> toIgnoreCaseSet

    ArbMap.defaults
    |> ArbMap.generate<string>
    |> Gen.filter (excludeSet.Contains >> not)

let generateStringFromDu<'DU> (exclude: string seq) =
    let excludeSet = exclude |> toIgnoreCaseSet

    ArbMap.defaults
    |> ArbMap.generate<string>
    |> Gen.filter (excludeSet.Contains >> not)

let findDu<'DU> (name: string) =
    FSharpType.GetUnionCases(typeof<'DU>)
    |> Seq.find (fun a -> a.Name.Equals(name, StringComparison.InvariantCultureIgnoreCase))
    |> fun x -> (FSharpValue.MakeUnion(x, [||]) :?> 'DU)

module ``Action fromString`` =
    let actionsNames = duNames<Action> |> Seq.filter ((<>) "AddQuery")

    type UnknownAction() =
        static member Generate() =
            Gen.frequency
                [ (1, randomCases "AddQuery" |> Gen.elements)
                  (9, actionsNames |> generateStringExclude) ]
            |> Arb.fromGen

    [<Tests>]
    let tests_unknownAction =
        let configUnknown =
            { FsCheckConfig.defaultConfig with
                arbitrary = [ typeof<UnknownAction> ] }

        testList
            "Action.fromString (unknown)"
            [

              testPropertyWithConfig configUnknown "When unknown action, should return error"
              <| fun (data: string) ->
                  data
                  |> Action.fromString
                  |> Expect.wantError "should return error"
                  |> Expect.equal "should return correct error message" $"Unknown Action '{data}'."
                  |> Prop.collect data

              ]

    type KnownAction() =
        static member Generate() =
            actionsNames |> Seq.collect randomCases |> Gen.elements |> Arb.fromGen

    [<Tests>]
    let tests_knownAction =
        let configKnown =
            { FsCheckConfig.defaultConfig with
                arbitrary = [ typeof<KnownAction> ] }

        testList
            "Action.fromString (known)"
            [

              testPropertyWithConfig configKnown "When known action, should return Ok"
              <| fun (data: string) ->
                  data
                  |> Action.fromString
                  |> Expect.equal "should return known actions excluding AddQuery" (data |> findDu<Action> |> Ok)
                  |> Prop.collect data

              ]

module fromString =
    let shouldFail<'DU when 'DU: equality> (fromString: string -> 'DU) value =
        Expect.throwsT<exn> "should throw for unknown value" (fun () -> fromString value |> ignore)

    let knownMatchers<'DU when 'DU: equality> (fromString: string -> 'DU) (data: string) =
        let actual: 'DU = fromString data
        let expected: 'DU = findDu<'DU> data
        actual |> Expect.equal "should return correct DU for known value" expected

    type InvalidDuName<'DU>() =
        static member Generate() =
            duNames<'DU> |> generateStringExclude |> Arb.fromGen

    type ValidDuName<'DU>() =
        static member Generate() =
            duNames<'DU> |> Seq.collect randomCases |> Gen.elements |> Arb.fromGen

    module ofMatcher =
        [<Tests>]
        let tests_ofMatcher =
            testList
                "Matcher.fromString"
                [

                  let configInvalid =
                      { FsCheckConfig.defaultConfig with
                          arbitrary = [ typeof<InvalidDuName<Matcher>> ] }

                  testPropertyWithConfig configInvalid "When unknown value, should throw" (fun data ->
                      shouldFail Matcher.fromString data |> Prop.collect data)

                  let configValid =
                      { FsCheckConfig.defaultConfig with
                          arbitrary = [ typeof<ValidDuName<Matcher>> ] }

                  testPropertyWithConfig configValid "When known value, should return correct DU" (fun data ->
                      knownMatchers Matcher.fromString data |> Prop.collect data)

                  ]

    module ofOperator =
        [<Tests>]
        let tests_ofOperator =
            testList
                "Operator.fromString"
                [

                  let configInvalid =
                      { FsCheckConfig.defaultConfig with
                          arbitrary = [ typeof<InvalidDuName<Operator>> ] }

                  testPropertyWithConfig configInvalid "When unknown value, should throw" (fun data ->
                      shouldFail Operator.fromString data |> Prop.collect data)

                  let configValid =
                      { FsCheckConfig.defaultConfig with
                          arbitrary = [ typeof<ValidDuName<Operator>> ] }

                  testPropertyWithConfig configValid "When known value, should return correct DU" (fun data ->
                      knownMatchers Operator.fromString data |> Prop.collect data)

                  ]

    module ofLayout =
        [<Tests>]
        let tests_ofLayout =
            testList
                "Layout.fromString"
                [

                  let configInvalid =
                      { FsCheckConfig.defaultConfig with
                          arbitrary = [ typeof<InvalidDuName<Layout>> ] }

                  testPropertyWithConfig configInvalid "When unknown value, should throw" (fun data ->
                      shouldFail Layout.fromString data |> Prop.collect data)

                  let configValid =
                      { FsCheckConfig.defaultConfig with
                          arbitrary = [ typeof<ValidDuName<Layout>> ] }

                  testPropertyWithConfig configValid "When known value, should return correct DU" (fun data ->
                      knownMatchers Layout.fromString data |> Prop.collect data)

                  ]

module ``QueryState toString`` =
    type QueryConditionGen() =
        static member Generate() =
            Gen.map4
                (fun m o c i ->
                    { Matcher = m
                      Operator = o
                      CaseSensitive = c
                      Invert = i })
                (ArbMap.generate<Matcher> ArbMap.defaults)
                (ArbMap.generate<Operator> ArbMap.defaults)
                (ArbMap.generate<bool> ArbMap.defaults)
                (ArbMap.generate<bool> ArbMap.defaults)
            |> Arb.fromGen

    [<Tests>]
    let tests_toString =
        let config =
            { FsCheckConfig.defaultConfig with
                arbitrary = [ typeof<QueryConditionGen> ] }

        testList
            "QueryState.toString"
            [

              testPropertyWithConfig config "When QueryCondition is given, should return correct format"
              <| fun (data: QueryCondition) ->
                  let c = if data.CaseSensitive then "c" else ""

                  let m =
                      match data.Matcher with
                      | Matcher.Eq -> if data.Invert then "ne" else "eq"
                      | Matcher.Like -> $"""{if data.Invert then "not" else ""}like"""
                      | Matcher.Match -> $"""{if data.Invert then "not" else ""}match"""

                  let o =
                      match data.Operator with
                      | Operator.And -> "and"
                      | Operator.Or -> "or"

                  data
                  |> QueryCondition.toString
                  |> Expect.equal "should return correct format" $"{c}{m} {o}"
                  |> Prop.collect data

              ]

module InternalState =
    [<Tests>]
    let tests_noRefresh =
        testList
            "InternalState.noRefresh"
            [

              test "When Refresh is NotRequired, should remain NotRequired" {
                  let state =
                      { QueryState =
                          { Query = ""
                            Cursor = 0
                            WindowBeginningCursor = 0
                            WindowWidth = 0
                            InputMode = InputMode.Input }
                        QueryCondition =
                          { Matcher = Matcher.Eq
                            Operator = Operator.And
                            CaseSensitive = false
                            Invert = false }
                        PropertySearch = PropertySearch.NoSearch
                        SuppressProperties = false
                        Refresh = Refresh.NotRequired
                        QueryCache = ValueNone }

                  state
                  |> InternalState.noRefresh
                  |> _.Refresh
                  |> Expect.equal "NotRequired" Refresh.NotRequired
              }

              ]


module initConfig =
    [<Tests>]
    let tests_initConfig =
        testList
            "InitConfig"
            [

              test "When valid config is provided" {

                  let actualConfig, actualState =
                      initConfig
                          { Query = ":name"
                            Matcher = "like"
                            Operator = "and"
                            CaseSensitive = true
                            InvertQuery = true
                            NotInteractive = true
                            SuppressProperties = true
                            Prompt = "prompt"
                            WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                            Layout = "TopDown"
                            Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, Action.Cancel) ]
                            Properties = [ "name"; "attributes" ]
                            PropertiesMap = Map [ ("name", "name"); ("attributes", "attributes") ]
                            ConsoleWidth = 60 }

                  let expectedConfig =
                      { Layout = Layout.TopDown
                        Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, Action.Cancel) ]
                        NotInteractive = true
                        WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                        Prompt = "prompt>"
                        PromptLength = 7
                        Properties = [ "name"; "attributes" ]
                        PropertiesMap = Map [ ("name", "name"); ("attributes", "attributes") ] }

                  let expectedState =
                      { QueryState =
                          { Query = ":name"
                            Cursor = 5
                            WindowBeginningCursor = 0
                            WindowWidth = 60 - (String.length "prompt>" + 1)
                            InputMode = InputMode.Input }
                        QueryCondition =
                          { Matcher = Matcher.Like
                            Operator = Operator.And
                            CaseSensitive = true
                            Invert = true }
                        PropertySearch = PropertySearch.Search "name"
                        SuppressProperties = true
                        Refresh = Refresh.Required
                        QueryCache = ValueNone }

                  actualConfig |> Expect.equal "should return correct config" expectedConfig
                  Helper.expectInternalStateEqual "should return expected state" expectedState actualState

              }

              test "When unknown Matcher is provided" {
                  Expect.throws "should fail" (fun () ->
                      initConfig
                          { Query = ""
                            Matcher = "same"
                            Operator = "or"
                            CaseSensitive = false
                            InvertQuery = false
                            NotInteractive = false
                            SuppressProperties = false
                            Prompt = "prompt"
                            WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                            Layout = "TopDown"
                            Keymaps = Map []
                            Properties = []
                            PropertiesMap = Map []
                            ConsoleWidth = 20 }
                      |> ignore)

              }

              test "When unknown Operator is provided" {
                  Expect.throws "should fail" (fun () ->
                      initConfig
                          { Query = ""
                            Matcher = "eq"
                            Operator = "not"
                            CaseSensitive = false
                            InvertQuery = false
                            NotInteractive = false
                            SuppressProperties = false
                            Prompt = "prompt"
                            WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                            Layout = "TopDown"
                            Keymaps = Map []
                            Properties = []
                            PropertiesMap = Map []
                            ConsoleWidth = 20 }

                      |> ignore)
              }

              test "When unknown Layout is provided" {
                  Expect.throws "should fail" (fun () ->
                      initConfig
                          { Query = ""
                            Matcher = "eq"
                            Operator = "or"
                            CaseSensitive = false
                            InvertQuery = false
                            NotInteractive = false
                            SuppressProperties = false
                            Prompt = "prompt"
                            WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                            Layout = "LeftToRight"
                            Keymaps = Map []
                            Properties = []
                            PropertiesMap = Map []
                            ConsoleWidth = 20 }
                      |> ignore)
              }

              ]

module QueryState =
    module getCurrentProperty =
        let qs q x =
            { Query = q
              Cursor = x
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }

        [<Tests>]
        let tests_getCurrentProperty =
            testList
                "getCurrentProperty"
                [

                  test "When no colon in query" {
                      QueryState.getCurrentProperty (qs "a" 1)
                      |> Expect.equal "should return PropertySearch.NoSearch" PropertySearch.NoSearch
                  }
                  test "When query starts with colon and cursor is at end" {
                      QueryState.getCurrentProperty (qs ":a" 2)
                      |> Expect.equal "should return PropertySearch.Search with 'a'" (PropertySearch.Search "a")
                  }
                  test "When query starts with colon and cursor is at position 1" {
                      QueryState.getCurrentProperty (qs ":a" 1)
                      |> Expect.equal "should return PropertySearch.Search with empty string" (PropertySearch.Search "")
                  }
                  test "When query starts with colon and has trailing space, cursor at 2" {
                      QueryState.getCurrentProperty (qs ":a " 2)
                      |> Expect.equal "should return PropertySearch.Search with 'a'" (PropertySearch.Search "a")
                  }
                  test "When query starts with colon and cursor is after space" {
                      QueryState.getCurrentProperty (qs ":a " 3)
                      |> Expect.equal "should return PropertySearch.NoSearch" (PropertySearch.NoSearch)
                  }
                  test "When query starts with colon and has trailing keyword, cursor at 2" {
                      QueryState.getCurrentProperty (qs ":a a" 2)
                      |> Expect.equal "should return PropertySearch.Search with 'a'" (PropertySearch.Search "a")
                  }

                  ]

    module deleteSelection =
        let state =
            { Query = ""
              Cursor = 0
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }

        [<Tests>]
        let tests_deleteSelection =
            testList
                "deleteSelection"
                [

                  test "When InputMode is Input, no selection" {
                      QueryState.deleteSelection state
                      |> Expect.equal "should return unchanged state" state
                  }

                  ]
