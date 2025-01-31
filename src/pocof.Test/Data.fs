module PocofTest.Data

open System
open Microsoft.FSharp.Reflection
open System.Collections.Generic

open Xunit
open FsUnitTyped
open FsCheck.FSharp
open FsCheck.Xunit

open Pocof
open Pocof.Data


module LanguageExtension =
    module Option =
        type Mock() =
            member val disposed = false with get, set

            interface IDisposable with
                member __.Dispose() = __.disposed <- true

        [<Fact>]
        let ``shouldn't call Dispose if Some.`` () =
            let mock = new Mock()
            mock |> Some |> Option.dispose
            mock.disposed |> shouldEqual true
            Option.dispose <| Some mock

        [<Fact>]
        let ``shouldn't call Dispose if None.`` () =
            // NOTE: only for coverage.
            None |> Option.dispose

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

    [<Property(Arbitrary = [| typeof<EntryPSObject> |], EndSize = 1000)>]
    let ``should return PSObject sequence.`` (data: Entry list) =
        data
        |> unwrap
        |> List.ofSeq
        |> shouldEqual (
            data
            |> List.map (function
                | Entry.Obj x -> x
                | _ -> failwith "Dict is unreachable")
        )
        |> Prop.collect (List.length data)

    type EntryDictionaryEntry =
        static member Generate() = dictionaryEntryGen |> Arb.fromGen

    [<Property(Arbitrary = [| typeof<EntryDictionaryEntry> |], EndSize = 1000)>]
    let ``should return DictionaryEntry sequence.`` (data: Entry list) =
        data
        |> unwrap
        |> List.ofSeq
        |> shouldEqual (
            data
            |> List.map (function
                | Entry.Dict x -> x
                | _ -> failwith "Obj is unreachable")
        )
        |> Prop.collect (List.length data)

    type MixedEntry =
        static member Generate() =
            Gen.oneof [ psObjectGen; dictionaryEntryGen ] |> Gen.listOf |> Arb.fromGen

    [<Property(Arbitrary = [| typeof<MixedEntry> |], EndSize = 1000)>]
    let ``should return mixed sequence.`` (data: Entry list) =
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

    [<Property(Arbitrary = [| typeof<UnknownAction> |])>]
    let ``should return unknown action error.`` (data: string) =
        data
        |> Action.fromString
        |> shouldEqual (Error $"Unknown Action '{data}'.")
        |> Prop.collect data

    type KnownAction() =
        static member Generate() =
            actionsNames |> Seq.collect randomCases |> Gen.elements |> Arb.fromGen

    [<Property(Arbitrary = [| typeof<KnownAction> |])>]
    let ``should return known actions excluding AddQuery.`` (data: string) =
        data
        |> Action.fromString
        |> shouldEqual (data |> findDu<Action> |> Ok)
        |> Prop.collect data

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
    [<Fact>]
    let ``should return tuples`` () =
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
        |> shouldEqual (
            { Layout = Layout.TopDown
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
              Refresh = Refresh.Required }
        )

    [<Fact>]
    let ``should fail due to unknown Matcher.`` () =
        shouldFail (fun () ->
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

    [<Fact>]
    let ``should fail due to unknown Operator.`` () =
        shouldFail (fun () ->
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

    [<Fact>]
    let ``should fail due to unknown Layout.`` () =
        shouldFail (fun () ->
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

module QueryState =
    module getCurrentProperty =
        let qs q x =
            { Query = q
              Cursor = x
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }

        [<Fact>]
        let ``should return NoSearch when no colon`` () =
            QueryState.getCurrentProperty (qs "a" 1) |> shouldEqual PropertySearch.NoSearch

        [<Fact>]
        let ``should return Search with "a" when start with colon`` () =
            QueryState.getCurrentProperty (qs ":a" 2)
            |> shouldEqual (PropertySearch.Search "a")

        [<Fact>]
        let ``should return Search with "a" when start with colon and cursor position 1`` () =
            QueryState.getCurrentProperty (qs ":a" 1)
            |> shouldEqual (PropertySearch.Search "")

        [<Fact>]
        let ``should return Search with "a" when start with colon and trailing space`` () =
            QueryState.getCurrentProperty (qs ":a " 2)
            |> shouldEqual (PropertySearch.Search "a")

        [<Fact>]
        let ``should return NoSearch when start with colon and cursor position 3`` () =
            QueryState.getCurrentProperty (qs ":a " 3)
            |> shouldEqual (PropertySearch.NoSearch)

        [<Fact>]
        let ``should return Search with "a" when start with colon and trailing keyword `` () =
            QueryState.getCurrentProperty (qs ":a a" 2)
            |> shouldEqual (PropertySearch.Search "a")

    module deleteSelection =
        [<Fact>]
        let ``should return no change when InputMode.Input `` () =
            let state =
                { Query = ""
                  Cursor = 0
                  WindowBeginningCursor = 0
                  WindowWidth = 0
                  InputMode = InputMode.Input }

            QueryState.deleteSelection state |> shouldEqual state
