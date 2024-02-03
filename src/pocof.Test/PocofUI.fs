module PocofUI

open Xunit
open FsUnitTyped
open System
open pocof.PocofData
open pocof.PocofQuery
open pocof.PocofScreen

let generateLine x y =
    List.replicate y <| String.replicate x " "

// TODO: mock PSHostRawUserInterface.
type MockRawUI =
    val caAsInput: bool
    val mutable height: int
    val mutable width: int
    val mutable x: int
    val mutable y: int
    val mutable screen: string list
    val mutable keys: ConsoleKeyInfo option list
    static xx = 50
    static yy = 30

    new() =
        // NOTE: accessing Console.TreatControlCAsInput will raise System.IO.IOException when running on GitHub Actions windows runner.
        { caAsInput = true
          x = MockRawUI.xx
          y = MockRawUI.yy
          width = MockRawUI.xx
          height = MockRawUI.yy
          screen = generateLine  MockRawUI.xx MockRawUI.yy
          keys = [MockRawUI.consoleKey '\000' ConsoleKey.Enter]
          }
    new(x:int, y:int) =
        // NOTE: accessing Console.TreatControlCAsInput will raise System.IO.IOException when running on GitHub Actions windows runner.
        { caAsInput = true
          x = x
          y = y
          width = x
          height = y
          screen = generateLine x y
          keys = [MockRawUI.consoleKey '\000' ConsoleKey.Enter]
          }

    new(x:int, y:int, keys: ConsoleKeyInfo option list) =
          // NOTE: accessing Console.TreatControlCAsInput will raise System.IO.IOException when running on GitHub Actions windows runner.
          { caAsInput = true
            x = x
            y = y
            width = x
            height = y
            screen = generateLine x y
            keys = keys
            }

    interface IRawUI with
        member __.GetCursorPosition () = __.x, __.y
        member __.SetCursorPosition (x: int) (y: int) =
            __.x <- x
            __.y <- y

        member __.GetLengthInBufferCells (s: string) =
            let isFullWidth c = // NOTE: simple full-width character detection for test.
                let code = int c
                code >= 0xFF00 && code <= 0xFF60

            s |> Seq.cast<char> |> Seq.map (fun c -> if isFullWidth c then 2 else 1) |> Seq.sum

        member __.GetWindowWidth() = __.width
        member __.GetWindowHeight() = __.height

        member __.Write x y s =
            __.screen <- __.screen |>List.mapi (fun i ss ->
                match i with
                | ii when ii = y ->
                    ss.Substring(0, x) + s
                | _ ->
                    let l = (__ :> IRawUI).GetLengthInBufferCells ss
                    match l <= __.width with
                    | true -> ss + String.replicate (__.width - l) " "
                    | _ -> ss)

        member __.ReadKey(_) =
            match __.keys with
            | [] -> failwith "key sequence is empty. check your test key sequence."
            | k::ks ->
                match k with
                | None -> failwith "key is none. check your test key sequence."
                | Some k ->
                    __.keys <- ks
                    k

        member __.KeyAvailable() =
            match __.keys with
            | [] -> false
            | k::ks ->
                match k with
                | None ->
                    __.keys <- ks
                    false
                | Some _ -> true

    interface IDisposable with
        member __.Dispose() = ()

    static member consoleKey keyChar key =
        new ConsoleKeyInfo(keyChar, key, false, false, false)
        |> Some

    member __.check() =
        match __.keys with
        | [] -> ()
        | _ -> failwith "keys remains. probably test is broken."

module ``Buff writeScreen`` =
    open System.Collections
    open System.Management.Automation

    [<Fact>]
    let ``should render top down.`` ()   =
        let rui = new MockRawUI()
        use buff = new Buff(rui,  (fun _ -> Seq.empty), TopDown)

        let state: InternalState =
            { QueryState = { Query = "foo"; Cursor = 3; WindowBeginningCursor = 0; WindowWidth = 0 }
              QueryCondition =
                { Matcher = MATCH
                  Operator = AND
                  CaseSensitive = true
                  Invert = false }
              PropertySearch = NoSearch
              Notification = ""
              SuppressProperties = false
              Properties = []
              Prompt = "query"
              FilteredCount = 0
              ConsoleWidth = rui.width
              Refresh = Required}
              |> InternalState.updateWindowWidth

        buff.writeScreen TopDown state [] <| Ok []

        let expected =
            "query>foo                           cmatch and [0]"
            :: (generateLine rui.width (rui.height - 1))

        rui.screen
        |> shouldEqual expected

    [<Fact>]
    let ``should render bottom up.`` ()   =
        let rui = new MockRawUI()
        use buff = new Buff(rui, (fun _ -> Seq.empty), BottomUp)

        let state: InternalState =
            { QueryState = { Query = "hello*world*"; Cursor = 12; WindowBeginningCursor = 0; WindowWidth = 0 }
              QueryCondition =
                { Matcher = LIKE
                  Operator = OR
                  CaseSensitive = false
                  Invert = true }
              PropertySearch = NoSearch
              Notification = ""
              SuppressProperties = false
              Properties = []
              Prompt = "prompt"
              FilteredCount = 0
              ConsoleWidth = rui.width
              Refresh = Required}
              |> InternalState.updateWindowWidth

        buff.writeScreen BottomUp state [] <| Ok []

        let expected =
            "prompt>hello*world*                 notlike or [0]"
            :: (generateLine rui.width (rui.height - 1))
            |> List.rev

        rui.screen
        |> shouldEqual expected

    [<Fact>]
    let ``should render notification.`` ()   =
        let rui = new MockRawUI(80,30)
        use buff = new Buff(rui,  (fun _ -> Seq.empty), TopDown)

        let state: InternalState =
            { QueryState = { Query = @"\"; Cursor = 1; WindowBeginningCursor = 0; WindowWidth = 0 }
              QueryCondition =
                { Matcher = MATCH
                  Operator = AND
                  CaseSensitive = false
                  Invert = false }
              PropertySearch = NoSearch
              Notification = ""
              SuppressProperties = false
              Properties = []
              Prompt = "prompt"
              FilteredCount = 0
              ConsoleWidth = rui.width
              Refresh = Required}
              |> InternalState.updateWindowWidth

        let state = state |> InternalState.prepareNotification

        buff.writeScreen TopDown state [] <| Ok []

        let expected =
            List.concat [ [ @"prompt>\                                                           match and [0]"
                            @"note>Invalid pattern '\' at offset 1. Illegal \ at end of pattern.              " ]
                          (generateLine rui.width (28)) ]

        rui.screen
        |> shouldEqual expected

    [<Fact>]
    let ``should render props notification.`` ()   =
        let rui = new MockRawUI(80,30)
        use buff = new Buff(rui,  (fun _ -> Seq.empty), TopDown)

        let state: InternalState =
            { QueryState = { Query = @":unknown"; Cursor = 8; WindowBeginningCursor = 0; WindowWidth = 0 }
              QueryCondition =
                { Matcher = MATCH
                  Operator = AND
                  CaseSensitive = false
                  Invert = false }
              PropertySearch = NoSearch
              Notification = ""
              SuppressProperties = false
              Properties = []
              Prompt = "prompt"
              FilteredCount = 0
              ConsoleWidth = rui.width
              Refresh = Required}
              |> InternalState.updateWindowWidth

        buff.writeScreen TopDown state [] <| Error "Property not found"

        let expected =
            List.concat [ [ @"prompt>:unknown                                                    match and [0]"
                            @"note>Property not found                                                         " ]
                          (generateLine rui.width (28)) ]

        rui.screen
        |> shouldEqual expected

    let formatTableOutString ol =
        PowerShell.Create().AddCommand("Format-Table").AddParameter("InputObject",ol).AddCommand("Out-String").Invoke() |> Seq.map string

    [<Fact>]
    let ``should render entries under y.`` ()   =
        let rui = new MockRawUI(60,30)
        use buff = new Buff(rui, formatTableOutString, TopDown)

        let state: InternalState =
            { QueryState = { Query = ""; Cursor = 0; WindowBeginningCursor = 0; WindowWidth = 0 }
              QueryCondition =
                { Matcher = MATCH
                  Operator = AND
                  CaseSensitive = false
                  Invert = false }
              PropertySearch = NoSearch
              Notification = ""
              SuppressProperties = false
              Properties = []
              Prompt = "prompt"
              FilteredCount = 10
              ConsoleWidth = rui.width
              Refresh = Required}
              |> InternalState.updateWindowWidth

        let entries = [1..10] |> List.map (fun i ->
            DictionaryEntry("Number", i) |> Dict
        )

        buff.writeScreen TopDown state entries <| Ok []

        let expected =
            List.concat [ [ @"prompt>                                       match and [10]"
                            @"                                                            "
                            @"                                                            "
                            @"Name                           Value                        "
                            @"----                           -----                        " ]
                          ([ 1..10 ]
                           |> List.map (sprintf "Number                         %-2d                           "))
                          (generateLine rui.width (15)) ]

        rui.screen
        |> shouldEqual expected


    [<Fact>]
    let ``should render entries over y.`` ()   =
        let rui = new MockRawUI(60,30)
        use buff = new Buff(rui, formatTableOutString, TopDown)

        let state: InternalState =
            { QueryState = { Query = ""; Cursor = 0; WindowBeginningCursor = 0; WindowWidth = 0 }
              QueryCondition =
                { Matcher = MATCH
                  Operator = AND
                  CaseSensitive = false
                  Invert = false }
              PropertySearch = NoSearch
              Notification = ""
              SuppressProperties = false
              Properties = []
              Prompt = "prompt"
              FilteredCount = 100
              ConsoleWidth = rui.width
              Refresh = Required}
              |> InternalState.updateWindowWidth

        let entries = [1..100] |> List.map (fun i ->
            DictionaryEntry("Number", i) |> Dict
        )
        buff.writeScreen TopDown state entries <| Ok []

        let expected =
            List.concat [ [ @"prompt>                                      match and [100]"
                            @"                                                            "
                            @"                                                            "
                            @"Name                           Value                        "
                            @"----                           -----                        " ]
                          ([ 1..25 ]
                           |> List.map (sprintf "Number                         %-2d                           ")) ]

        rui.screen
        |> shouldEqual expected

    module ``query window`` =
        let getRenderedScreen query cursor beginning =
            let rui = new MockRawUI(50,25)
            use buff = new Buff(rui, (fun _ -> Seq.empty), TopDown)
            let state: InternalState =
                { QueryState =
                      { Query = query
                        Cursor = cursor
                        WindowBeginningCursor = beginning
                        WindowWidth = 30 }
                  QueryCondition =
                      { Matcher = MATCH
                        Operator = AND
                        CaseSensitive = false
                        Invert = false }
                  PropertySearch = NoSearch
                  Notification = ""
                  SuppressProperties = false
                  Properties = []
                  Prompt = "query"
                  FilteredCount = 0
                  ConsoleWidth = rui.width
                  Refresh = Required}
            buff.writeScreen TopDown state [] <| Ok []
            rui

        [<Fact>]
        let ``should render head 30 of query when cursor 0.`` ()   =
            let query = [0..9] |> List.map (sprintf "%d-------->") |> String.concat ""
            let rui = getRenderedScreen query 0 0

            let expected =
                "query>0-------->1-------->2--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

        [<Fact>]
        let ``should render head 30 of query when cursor 30.`` ()   =
            let query = [0..9] |> List.map (sprintf "%d-------->") |> String.concat ""
            let rui = getRenderedScreen query 30 0

            let expected =
                "query>0-------->1-------->2--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

        [<Fact>]
        let ``should render mid 30 of query when cursor 45.`` ()   =
            let query = [0..9] |> List.map (sprintf "%d-------->") |> String.concat ""
            let rui = getRenderedScreen query 45 15

            let expected =
                "query>---->2-------->3-------->4---- match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

        [<Fact>]
        let ``should render tail 30 of query when cursor 100.`` ()   =
            let query = [0..9] |> List.map (sprintf "%d-------->") |> String.concat ""
            let rui = getRenderedScreen query 100 70

            let expected =
                "query>7-------->8-------->9--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

        let intToChar i = Char.ConvertFromUtf32(i + Char.ConvertToUtf32("０",0))

        [<Fact>]
        let ``should render head 30 of query when cursor 0 with full-width characters.`` ()   =
            let query = [0..9] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""
            let rui = getRenderedScreen query 0 0

            let expected =
                "query>０------->１------->２-------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

        [<Fact>]
        let ``should render head 30 of query when cursor 30 with full-width characters.`` ()   =
            let query = [0..9] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""
            let rui = getRenderedScreen query 27 0

            let expected =
                "query>０------->１------->２-------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

        [<Fact>]
        let ``should render mid 30 of query when cursor 45 with full-width characters.`` ()   =
            let query = [0..9] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""
            let rui = getRenderedScreen query 40 13

            let expected =
                "query>---->２------->３------->４--- match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

        [<Fact>]
        let ``should render tail 30 of query when cursor 100 with full-width characters.`` ()   =
            let query = [0..9] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""
            let rui = getRenderedScreen query 90 63

            let expected =
                "query>７------->８------->９-------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

module ``Buff getConsoleWidth`` =
    [<Fact>]
    let ``should render top down.`` ()   =
        let rui = new MockRawUI(60,30)
        use buff = new Buff(rui,  (fun _ -> Seq.empty), TopDown)
        buff.getConsoleWidth() |> shouldEqual 60

module ``Buff getKey`` =
    [<Fact>]
    let ``should render top down.`` ()   =
        let rui = new MockRawUI()
        use buff = new Buff(rui,  (fun _ -> Seq.empty), TopDown)
        let expected = [new ConsoleKeyInfo('\000', ConsoleKey.Enter, false, false, false)]
        buff.getKey() |> shouldEqual expected
