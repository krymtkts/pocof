module PocofTest.Data

open System
open Microsoft.FSharp.Reflection
open System.Collections.Generic

open FsUnitTyped
open FsCheck.FSharp
open FsCheck.Xunit

open Pocof
open Pocof.Data

open Expecto
open Expecto.Flip

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
                  a["Dummy"] |> Expect.equal "should return None" None
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
                  |> shouldEqual (
                      data
                      |> List.map (function
                          | Entry.Obj x -> x
                          | Entry.Dict x -> x)
                  )
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
                  |> Expect.equal "should return unknown action error" (Error $"Unknown Action '{data}'.")
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
    let ``should fail.``<'DU> (fromString: string -> 'DU) value =
        shouldFail (fun () -> fromString value |> ignore)

    let ``known matchers.``<'DU> (fromString: string -> 'DU) (data: string) =
        data |> fromString |> shouldEqual (data |> findDu<'DU>)

    type InvalidDuName<'DU>() =
        static member Generate() =
            duNames<'DU> |> generateStringExclude |> Arb.fromGen

    type ValidDuName<'DU>() =
        static member Generate() =
            duNames<'DU> |> Seq.collect randomCases |> Gen.elements |> Arb.fromGen

    module ``of Matcher`` =
        [<Property(Arbitrary = [| typeof<InvalidDuName<Matcher>> |])>]
        let ``should fail when unknown value.`` (data: string) =
            data |> ``should fail.`` Matcher.fromString

        [<Property(Arbitrary = [| typeof<ValidDuName<Matcher>> |])>]
        let ``should return known matchers.`` (data: string) =
            data |> ``known matchers.`` Matcher.fromString |> Prop.collect data

    module ``of Operator`` =
        [<Property(Arbitrary = [| typeof<InvalidDuName<Operator>> |])>]
        let ``should fail when unknown value.`` (data: string) =
            data |> ``should fail.`` Operator.fromString

        [<Property(Arbitrary = [| typeof<ValidDuName<Operator>> |])>]
        let ``should return known matchers.`` (data: string) =
            data |> ``known matchers.`` Operator.fromString |> Prop.collect data

    module ``of Layout`` =
        [<Property(Arbitrary = [| typeof<InvalidDuName<Layout>> |])>]
        let ``should fail when unknown value.`` (data: string) =
            data |> ``should fail.`` Layout.fromString

        [<Property(Arbitrary = [| typeof<ValidDuName<Layout>> |])>]
        let ``should return known matchers.`` (data: string) =
            data |> ``known matchers.`` Layout.fromString |> Prop.collect data

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

    [<Property(Arbitrary = [| typeof<QueryConditionGen> |])>]
    let ``should return correct format.`` (data: QueryCondition) =
        let c = if data.CaseSensitive then "c" else ""

        let m =
            match data.Matcher with
            | Matcher.Eq -> if data.Invert then "ne" else "eq"
            | Matcher.Like -> $"""{if data.Invert then "not" else ""}like"""
            | Matcher.Match -> $"""{if data.Invert then "not" else ""}match"""

        data
        |> QueryCondition.toString
        |> shouldEqual $"{c}{m} {data.Operator}"
        |> Prop.collect data

module initConfig =
    [<Tests>]
    let tests_initConfig =
        testList
            "InitConfig"
            [

              test "When valid config is provided" {

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
                  |> Expect.equal
                      "should return tuples"
                      ({ Layout = Layout.TopDown
                         Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, Action.Cancel) ]
                         NotInteractive = true
                         WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
                         Prompt = "prompt>"
                         PromptLength = 7
                         Properties = [ "name"; "attributes" ]
                         PropertiesMap = Map [ ("name", "name"); ("attributes", "attributes") ] },
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
                         Refresh = Refresh.Required })

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
