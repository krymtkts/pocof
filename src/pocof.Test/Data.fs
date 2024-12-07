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
            // TODO: use .Is* after bumping to F# 9.
            data
            |> List.filter (function
                | Entry.Obj _ -> true
                | _ -> false)
            |> List.length,
            data
            |> List.filter (function
                | Entry.Dict _ -> true
                | _ -> false)
            |> List.length
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
    [ name; String.lower name; String.upper name; randomCase name ]

let toIgnoreCaseSet (x: string seq) =
    HashSet(x, StringComparer.InvariantCultureIgnoreCase)

module ``Action fromString`` =
    let actionsNames = duNames<Action> |> Seq.filter ((<>) "AddQuery")
    let actionsNameSet = actionsNames |> toIgnoreCaseSet

    type UnknownAction() =
        static member Generate() =
            Gen.frequency
                [ (1, Gen.elements <| randomCases "AddQuery")
                  (9, ArbMap.defaults |> ArbMap.generate<string>) ]
            |> Arb.fromGen
            |> Arb.filter (actionsNameSet.Contains >> not)

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
        let a =
            FSharpType.GetUnionCases(typeof<Action>)
            |> Seq.find (fun a -> a.Name.Equals(data, StringComparison.InvariantCultureIgnoreCase))

        data
        |> Action.fromString
        |> shouldEqual (Ok(FSharpValue.MakeUnion(a, [||]) :?> Action))
        |> Prop.collect data

module fromString =
    let ``should fail.``<'a> (fromString: string -> 'a) value =
        shouldFail (fun () -> fromString value |> ignore)

    let ``known matchers.``<'a> (fromString: string -> 'a) =
        FSharpType.GetUnionCases(typeof<'a>)
        |> Seq.iter (fun (a: UnionCaseInfo) ->
            a.Name
            |> randomCases
            |> List.map fromString
            |> List.iter (shouldEqual (FSharpValue.MakeUnion(a, [||]) :?> 'a)))

    type ExcludeGen<'U>() =
        static member Generate() =
            let matchers = duNames<'U> |> toIgnoreCaseSet

            ArbMap.defaults
            |> ArbMap.generate<string>
            |> Arb.fromGen
            |> Arb.filter (matchers.Contains >> not)

    module ``of Matcher`` =
        [<Property(Arbitrary = [| typeof<ExcludeGen<Matcher>> |])>]
        let ``should fail when unknown value.`` (data: string) =
            data |> ``should fail.``<Matcher> Matcher.fromString

        [<Fact>]
        let ``should return known matchers.`` () =
            ``known matchers.``<Matcher> Matcher.fromString

    module ``of Operator`` =
        [<Property(Arbitrary = [| typeof<ExcludeGen<Operator>> |])>]
        let ``should fail when unknown value.`` (data: string) =
            data |> ``should fail.``<Operator> Operator.fromString

        [<Fact>]
        let ``should return known matchers.`` () =
            ``known matchers.``<Operator> Operator.fromString

    module ``of Layout`` =
        [<Property(Arbitrary = [| typeof<ExcludeGen<Layout>> |])>]
        let ``should fail when unknown value.`` (data: string) =
            data |> ``should fail.``<Layout> Layout.fromString

        [<Fact>]
        let ``should return known matchers.`` () =
            ``known matchers.``<Layout> Layout.fromString

module ``QueryState toString`` =
    let queryState (m: Matcher) (o: Operator) : QueryCondition =
        { Matcher = m
          Operator = o
          CaseSensitive = false
          Invert = false }

    let caseSensitive (s: QueryCondition) = { s with CaseSensitive = true }
    let invert (s: QueryCondition) = { s with Invert = true }

    [<Fact>]
    let ``should return eq and`` () =
        let actual = queryState Matcher.Eq Operator.And
        string actual |> shouldEqual "eq and"

    [<Fact>]
    let ``should return cne or`` () =
        let actual = queryState Matcher.Eq Operator.Or |> caseSensitive |> invert

        string actual |> shouldEqual "cne or"

    [<Fact>]
    let ``should return ceq and`` () =
        let actual = queryState Matcher.Eq Operator.And |> caseSensitive

        string actual |> shouldEqual "ceq and"

    [<Fact>]
    let ``should return ne or`` () =
        let actual = queryState Matcher.Eq Operator.Or |> invert
        string actual |> shouldEqual "ne or"

    [<Fact>]
    let ``should return like and`` () =
        let actual = queryState Matcher.Like Operator.And
        string actual |> shouldEqual "like and"

    [<Fact>]
    let ``should return clike and`` () =
        let actual = queryState Matcher.Like Operator.And |> caseSensitive

        string actual |> shouldEqual "clike and"

    [<Fact>]
    let ``should return notlike and`` () =
        let actual = queryState Matcher.Like Operator.And |> invert
        string actual |> shouldEqual "notlike and"

    [<Fact>]
    let ``should return cnotlike and`` () =
        let actual = queryState Matcher.Like Operator.And |> caseSensitive |> invert

        string actual |> shouldEqual "cnotlike and"

    [<Fact>]
    let ``should return cnotmatch or`` () =
        let actual = queryState Matcher.Match Operator.Or |> caseSensitive |> invert

        string actual |> shouldEqual "cnotmatch or"

    [<Fact>]
    let ``should return notmatch or`` () =
        let actual = queryState Matcher.Match Operator.Or |> invert
        string actual |> shouldEqual "notmatch or"

    [<Fact>]
    let ``should return cmatch or`` () =
        let actual = queryState Matcher.Match Operator.Or |> caseSensitive

        string actual |> shouldEqual "cmatch or"

    [<Fact>]
    let ``should return match or`` () =
        let actual = queryState Matcher.Match Operator.Or
        string actual |> shouldEqual "match or"

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
              EntryCount = 10
              ConsoleWidth = 60
              ConsoleHeight = 20 }
        |> shouldEqual (
            { Layout = Layout.TopDown
              Keymaps = Map [ ({ Modifier = 7; Key = ConsoleKey.X }, Action.Cancel) ]
              NotInteractive = true },
            { QueryState =
                { Query = ":name"
                  Cursor = 5
                  WindowBeginningCursor = 0
                  WindowWidth = 60 - (String.length "prompt>") - (String.length " cnotlike and [10]")
                  InputMode = InputMode.Input }
              QueryCondition =
                { Matcher = Matcher.Like
                  Operator = Operator.And
                  CaseSensitive = true
                  Invert = true }
              PropertySearch = PropertySearch.Search "name"
              Notification = ""
              SuppressProperties = true
              Properties = [ "name"; "attributes" ]
              PropertyMap = Map [ ("name", "name"); ("attributes", "attributes") ]
              Prompt = "prompt"
              WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
              FilteredCount = 10
              ConsoleWidth = 60
              Refresh = Refresh.Required },
            { Y = 0; Height = 20 }
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
                  EntryCount = 10
                  ConsoleWidth = 20
                  ConsoleHeight = 20 }
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
                  EntryCount = 10
                  ConsoleWidth = 20
                  ConsoleHeight = 20 }

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
                  EntryCount = 10
                  ConsoleWidth = 20
                  ConsoleHeight = 20 }
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
