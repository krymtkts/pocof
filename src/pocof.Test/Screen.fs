module PocofTest.Screen

open System

open Xunit
open FsUnitTyped

open Pocof.Data
open Pocof.LanguageExtension
open Pocof.Screen

[<AutoOpen>]
module Mock =
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
        val mutable forceCancel: bool
        static member xx = 50
        static member yy = 30

        new() =
            // NOTE: accessing Console.TreatControlCAsInput will raise System.IO.IOException when running on GitHub Actions windows runner.
            { caAsInput = true
              x = MockRawUI.xx
              y = MockRawUI.yy
              width = MockRawUI.xx
              height = MockRawUI.yy
              screen = generateLine MockRawUI.xx MockRawUI.yy
              keys = [ MockRawUI.ConsoleKey '\000' ConsoleKey.Enter ]
              forceCancel = false }

        new(x: int, y: int) =
            // NOTE: accessing Console.TreatControlCAsInput will raise System.IO.IOException when running on GitHub Actions windows runner.
            { caAsInput = true
              x = x
              y = y
              width = x
              height = y
              screen = generateLine x y
              keys = [ MockRawUI.ConsoleKey '\000' ConsoleKey.Enter ]
              forceCancel = false }

        new(x: int, y: int, keys: ConsoleKeyInfo option list) =
            // NOTE: accessing Console.TreatControlCAsInput will raise System.IO.IOException when running on GitHub Actions windows runner.
            { caAsInput = true
              x = x
              y = y
              width = x
              height = y
              screen = generateLine x y
              keys = keys
              forceCancel = false }

        new(x: int, y: int, keys: ConsoleKeyInfo option list, forceCancel: bool) =
            // NOTE: accessing Console.TreatControlCAsInput will raise System.IO.IOException when running on GitHub Actions windows runner.
            { caAsInput = true
              x = x
              y = y
              width = x
              height = y
              screen = generateLine x y
              keys = keys
              forceCancel = forceCancel }

        interface IRawUI with
            member __.GetCursorPosition() = __.x, __.y

            member __.SetCursorPosition (x: int) (y: int) =
                __.x <- x
                __.y <- y

            member __.GetLengthInBufferCells(s: string) =
                let isFullWidth c = // NOTE: simple full-width character detection for test.
                    let code = int c
                    code >= 0xFF00 && code <= 0xFF60

                s |> Seq.cast<char> |> Seq.sumBy (fun c -> if isFullWidth c then 2 else 1)

            member __.GetWindowWidth() = __.width
            member __.GetWindowHeight() = __.height

            member __.Write x y s =
                __.screen <-
                    __.screen
                    |> List.mapi (fun i ss ->
                        match i with
                        | ii when ii = y -> ss.Substring(0, x) + s
                        | _ ->
                            let l = (__ :> IRawUI).GetLengthInBufferCells ss

                            match l <= __.width with
                            | true -> ss + String.replicate (__.width - l) " "
                            | _ -> ss)

            member __.ReadKey(_) =
                match __.keys with
                | [] -> failwith "key sequence is empty. check your test key sequence."
                | k :: ks ->
                    match k with
                    | None -> failwith "key is none. check your test key sequence."
                    | Some k ->
                        __.keys <- ks
                        k

            member __.KeyAvailable() =
                match __.keys with
                | [] ->
                    if __.forceCancel then
                        __.keys <- [ MockRawUI.ConsoleKey '\000' ConsoleKey.Escape ]
                        __.forceCancel <- false

                    false
                | k :: ks ->
                    match k with
                    | None ->
                        __.keys <- ks
                        false
                    | Some _ -> true

            member __.HideCursorWhileRendering() =
                { new IDisposable with
                    member _.Dispose() = () }

        interface IDisposable with
            member __.Dispose() = ()

        static member ConsoleKey keyChar key =
            new ConsoleKeyInfo(keyChar, key, false, false, false) |> Some

        member __.Check() =
            match __.keys with
            | [] -> ()
            | _ -> failwith "keys remains. probably test is broken."

module ``Buff writeScreen`` =
    open System.Collections
    open System.Management.Automation

    let state: InternalState =
        { QueryState =
            { Query = "foo"
              Cursor = 3
              WindowBeginningCursor = 0
              WindowWidth = 0
              InputMode = InputMode.Input }
          QueryCondition =
            { Matcher = Matcher.Match
              Operator = Operator.And
              CaseSensitive = true
              Invert = false }
          PropertySearch = PropertySearch.NoSearch
          Notification = ""
          SuppressProperties = false
          Properties = []
          PropertyMap = Map []
          Prompt = "query"
          WordDelimiters = ";:,.[]{}()/\\|!?^&*-=+'\"–—―"
          FilteredCount = 0
          ConsoleWidth = 0
          Refresh = Refresh.Required }

    let getRenderedScreen rui state layout =
        // NOTE: avoid cleanup of buff to check screen.
        let buff = new Buff(rui, (fun _ -> Seq.empty), layout)
        buff.WriteScreen layout state PSeq.empty <| Ok []
        rui

    [<Fact>]
    let ``should render top down.`` () =
        let rui = new MockRawUI()

        let state =
            { state with ConsoleWidth = rui.width } |> InternalState.updateWindowWidth

        let rui = getRenderedScreen rui state Layout.TopDown

        let expected =
            "query>foo                           cmatch and [0]"
            :: (generateLine rui.width (rui.height - 1))

        rui.screen |> shouldEqual expected

    [<Fact>]
    let ``should render top down half.`` () =
        let rui = new MockRawUI()

        (rui :> IRawUI).GetCursorPosition()
        |> (fun (x, y) -> (rui :> IRawUI).SetCursorPosition <| x / 2 <| y / 2 + 1)

        let state =
            { state with ConsoleWidth = rui.width } |> InternalState.updateWindowWidth

        let rui = getRenderedScreen rui state Layout.TopDownHalf

        let expected =
            [ generateLine rui.width (rui.height / 2)
              [ "query>foo                           cmatch and [0]" ]
              generateLine rui.width (rui.height / 2 - 1) ]
            |> List.concat

        rui.screen |> shouldEqual expected

    [<Fact>]
    let ``should render bottom up.`` () =
        let rui = new MockRawUI()

        let state =
            { state with
                QueryState =
                    { Query = "hello*world*"
                      Cursor = 12
                      WindowBeginningCursor = 0
                      WindowWidth = 0
                      InputMode = InputMode.Input }
                QueryCondition =
                    { Matcher = Matcher.Like
                      Operator = Operator.Or
                      CaseSensitive = false
                      Invert = true }
                Prompt = "prompt"
                ConsoleWidth = rui.width }
            |> InternalState.updateWindowWidth

        let rui = getRenderedScreen rui state Layout.BottomUp

        let expected =
            "prompt>hello*world*                 notlike or [0]"
            :: (generateLine rui.width (rui.height - 1))
            |> List.rev

        rui.screen |> shouldEqual expected

    [<Fact>]
    let ``should render bottom up half.`` () =
        let rui = new MockRawUI()

        let state: InternalState =
            { state with
                InternalState.QueryState.Query = "hello*world*"
                InternalState.QueryState.Cursor = 12
                QueryCondition =
                    { Matcher = Matcher.Like
                      Operator = Operator.Or
                      CaseSensitive = false
                      Invert = true }
                Prompt = "prompt"
                ConsoleWidth = rui.width }
            |> InternalState.updateWindowWidth

        let rui = getRenderedScreen rui state Layout.BottomUpHalf

        let expected =
            "prompt>hello*world*                 notlike or [0]"
            :: (generateLine rui.width (rui.height - 1))
            |> List.rev

        rui.screen |> shouldEqual expected

    [<Fact>]
    let ``should render notification.`` () =
        let rui = new MockRawUI(80, 30)

        let state: InternalState =
            { state with
                InternalState.QueryState.Query = @"\"
                InternalState.QueryState.Cursor = 1
                InternalState.QueryCondition.CaseSensitive = false
                Prompt = "prompt"
                ConsoleWidth = rui.width }
            |> InternalState.updateWindowWidth
            |> Pocof.Query.InternalState.prepareNotification

        let rui = getRenderedScreen rui state Layout.TopDown

        let expected =
            List.concat
                [ [ @"prompt>\                                                           match and [0]"
                    @"note>Invalid pattern '\' at offset 1. Illegal \ at end of pattern.              " ]
                  (generateLine rui.width (28)) ]

        rui.screen |> shouldEqual expected

    [<Fact>]
    let ``should render property suggestions.`` () =
        let rui = new MockRawUI(80, 30)
        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)
        let props = [ 1..20 ] |> List.map (fun i -> $"Name%02d{i}")

        let state: InternalState =
            { state with
                InternalState.QueryState.Query = @":"
                InternalState.QueryState.Cursor = 1
                InternalState.QueryCondition.CaseSensitive = false
                PropertySearch = PropertySearch.Search("")
                Properties = props
                PropertyMap = props |> List.map (fun s -> (s.ToLower(), s)) |> Map
                Prompt = "prompt"
                ConsoleWidth = rui.width }
            |> InternalState.updateWindowWidth
            |> Pocof.Query.InternalState.prepareNotification

        buff.WriteScreen Layout.TopDown state PSeq.empty
        <| (state.Properties |> List.ofSeq |> Ok)

        let expected =
            List.concat
                [ [ @"prompt>:                                                           match and [0]"
                    @"Name01 Name02 Name03 Name04 Name05 Name06 Name07 Name08 Name09 Name10 Name11 Nam" ]
                  (generateLine rui.width (28)) ]

        rui.screen |> shouldEqual expected

    [<Fact>]
    let ``should render props notification.`` () =
        let rui = new MockRawUI(80, 30)
        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)

        let state: InternalState =
            { state with
                InternalState.QueryState.Query = @":unknown"
                InternalState.QueryState.Cursor = 8
                InternalState.QueryCondition.CaseSensitive = false
                Prompt = "prompt"
                ConsoleWidth = rui.width }
            |> InternalState.updateWindowWidth

        buff.WriteScreen Layout.TopDown state PSeq.empty <| Error "Property not found"

        let expected =
            List.concat
                [ [ @"prompt>:unknown                                                    match and [0]"
                    @"note>Property not found                                                         " ]
                  (generateLine rui.width (28)) ]

        rui.screen |> shouldEqual expected

    let formatTableOutString ol =
        PowerShell
            .Create()
            .AddCommand("Format-Table")
            .AddParameter("InputObject", ol)
            .AddCommand("Out-String")
            .Invoke()
        |> Seq.map string

    [<Fact>]
    let ``should render entries under y.`` () =
        let rui = new MockRawUI(60, 30)
        use buff = new Buff(rui, formatTableOutString, Layout.TopDown)

        let state: InternalState =
            { state with
                InternalState.QueryState.Query = ""
                InternalState.QueryState.Cursor = 0
                InternalState.QueryCondition.CaseSensitive = false
                Prompt = "prompt"
                FilteredCount = 10
                ConsoleWidth = rui.width }
            |> InternalState.updateWindowWidth

        let entries =
            [ 1..10 ]
            |> List.map (fun i -> DictionaryEntry("Number", i) |> Entry.Dict)
            |> PSeq.ofSeq

        buff.WriteScreen Layout.TopDown state entries <| Ok []

        let expected =
            List.concat
                [ [ @"prompt>                                       match and [10]"
                    @"                                                            "
                    @"                                                            "
                    @"Name                           Value                        "
                    @"----                           -----                        " ]
                  ([ 1..10 ]
                   |> List.map (sprintf "Number                         %-2d                           "))
                  (generateLine rui.width (15)) ]

        rui.screen |> shouldEqual expected


    [<Fact>]
    let ``should render entries over y.`` () =
        let rui = new MockRawUI(60, 30)
        use buff = new Buff(rui, formatTableOutString, Layout.TopDown)

        let state: InternalState =
            { state with
                InternalState.QueryState.Query = ""
                InternalState.QueryState.Cursor = 0
                InternalState.QueryCondition.CaseSensitive = false
                Prompt = "prompt"
                FilteredCount = 100
                ConsoleWidth = rui.width }
            |> InternalState.updateWindowWidth

        let entries =
            [ 1..100 ]
            |> List.map (fun i -> DictionaryEntry("Number", i) |> Entry.Dict)
            |> PSeq.ofSeq

        buff.WriteScreen Layout.TopDown state entries <| Ok []

        let expected =
            List.concat
                [ [ @"prompt>                                      match and [100]"
                    @"                                                            "
                    @"                                                            "
                    @"Name                           Value                        "
                    @"----                           -----                        " ]
                  ([ 1..25 ]
                   |> List.map (sprintf "Number                         %-2d                           ")) ]

        rui.screen |> shouldEqual expected

    module ``query window`` =
        let getRenderedScreen query cursor beginning =
            let rui = new MockRawUI(50, 25)
            // NOTE: avoid cleanup of buff to check screen.
            let state =
                { state with
                    QueryState =
                        { Query = query
                          Cursor = cursor
                          WindowBeginningCursor = beginning
                          WindowWidth = 30
                          InputMode = InputMode.Input }
                    InternalState.QueryCondition.CaseSensitive = false
                    ConsoleWidth = rui.width }

            getRenderedScreen rui state Layout.TopDown

        [<Fact>]
        let ``should render head 30 of query when cursor 0.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 0 0

            let expected =
                "query>0-------->1-------->2--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render head 30 of query when cursor 30.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 30 0

            let expected =
                "query>0-------->1-------->2--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render mid 30 of query when cursor 45.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 45 15

            let expected =
                "query>---->2-------->3-------->4---- match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render tail 30 of query when cursor 100.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 100 70

            let expected =
                "query>7-------->8-------->9--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        let intToChar i =
            Char.ConvertFromUtf32(i + Char.ConvertToUtf32("０", 0))

        [<Fact>]
        let ``should render head 30 of query when cursor 0 with full-width characters.`` () =
            let query =
                [ 0..9 ] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""

            let rui = getRenderedScreen query 0 0

            let expected =
                "query>０------->１------->２-------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render head 30 of query when cursor 30 with full-width characters.`` () =
            let query =
                [ 0..9 ] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""

            let rui = getRenderedScreen query 27 0

            let expected =
                "query>０------->１------->２-------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render mid 30 of query when cursor 45 with full-width characters.`` () =
            let query =
                [ 0..9 ] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""

            let rui = getRenderedScreen query 40 13

            let expected =
                "query>---->２------->３------->４--- match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render tail 30 of query when cursor 100 with full-width characters.`` () =
            let query =
                [ 0..9 ] |> List.map (intToChar >> sprintf "%s------->") |> String.concat ""

            let rui = getRenderedScreen query 90 63

            let expected =
                "query>７------->８------->９-------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

    module ``query selection`` =
        let getRenderedScreen query cursor beginning inputMode =
            let rui = new MockRawUI(50, 25)
            // NOTE: avoid cleanup of buff to check screen.
            let state =
                { state with
                    QueryState =
                        { Query = query
                          Cursor = cursor
                          WindowBeginningCursor = beginning
                          WindowWidth = 30
                          InputMode = inputMode }
                    InternalState.QueryCondition.CaseSensitive = false
                    ConsoleWidth = rui.width }

            getRenderedScreen rui state Layout.TopDown

        [<Fact>]
        let ``should render query without selection.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 0 0 InputMode.Input

            let expected =
                "query>0-------->1-------->2--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render query with cursor at 0 and selection from 0 to 10.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 0 0 <| InputMode.Select(-10)

            let expected =
                $"query>{escapeSequenceInvert}0-------->{escapeSequenceResetInvert}1-------->2--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render query with cursor at 10 and selection from 0 to 10.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 10 0 <| InputMode.Select(10)

            let expected =
                $"query>{escapeSequenceInvert}0-------->{escapeSequenceResetInvert}1-------->2--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render query with cursor at 0 and selection from 0 to 40.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 0 0 <| InputMode.Select(-40)

            let expected =
                $"query>{escapeSequenceInvert}0-------->1-------->2-------->{escapeSequenceResetInvert} match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render query with cursor at 30 and selection from 30 to 40.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 30 30 <| InputMode.Select(-10)

            let expected =
                $"query>{escapeSequenceInvert}3-------->{escapeSequenceResetInvert}4-------->5--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render query with cursor at 40 and selection from 30 to 40.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 40 30 <| InputMode.Select(10)

            let expected =
                $"query>{escapeSequenceInvert}3-------->{escapeSequenceResetInvert}4-------->5--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render query with cursor at 60 and selection from 20 to 60.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 60 30 <| InputMode.Select(40)

            let expected =
                $"query>{escapeSequenceInvert}3-------->4-------->5-------->{escapeSequenceResetInvert} match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render query with cursor at 90 and selection from 90 to 100.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 90 70 <| InputMode.Select(-10)

            let expected =
                $"query>7-------->8-------->{escapeSequenceInvert}9-------->{escapeSequenceResetInvert} match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render query with cursor at 100 and selection from 90 to 100.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 100 70 <| InputMode.Select(10)

            let expected =
                $"query>7-------->8-------->{escapeSequenceInvert}9-------->{escapeSequenceResetInvert} match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

        [<Fact>]
        let ``should render query with cursor at 100 and selection from 60 to 100.`` () =
            let query = [ 0..9 ] |> List.map (sprintf "%d-------->") |> String.concat ""

            let rui = getRenderedScreen query 100 70 <| InputMode.Select(40)

            let expected =
                $"query>{escapeSequenceInvert}7-------->8-------->9-------->{escapeSequenceResetInvert} match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen |> shouldEqual expected

module ``Buff getConsoleWidth`` =
    [<Fact>]
    let ``should render top down.`` () =
        let rui = new MockRawUI(60, 30)
        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)
        buff.GetConsoleWidth() |> shouldEqual 60

module ``Buff getKey`` =
    [<Fact>]
    let ``should render top down.`` () =
        let rui = new MockRawUI()
        use buff = new Buff(rui, (fun _ -> Seq.empty), Layout.TopDown)
        let expected = [ new ConsoleKeyInfo('\000', ConsoleKey.Enter, false, false, false) ]
        buff.GetKey() |> shouldEqual expected
