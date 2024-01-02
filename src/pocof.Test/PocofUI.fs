module PocofUI

open Xunit
open FsUnitTyped
open System
open pocof.PocofData
open pocof.PocofScreen

let generateLine x y =
    List.replicate y <| String.replicate x " "

// TODO: mock PSHostRawUserInterface.
type MockRawUI =
    val caAsInput: bool
    val height: int
    val width: int
    val mutable x: int
    val mutable y: int
    val mutable screen: string list

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
          }
    new(x:int, y:int) =
        // NOTE: accessing Console.TreatControlCAsInput will raise System.IO.IOException when running on GitHub Actions windows runner.
        { caAsInput = true
          x = x
          y = y
          width = x
          height = y
          screen = generateLine x y
          }

    interface IRawUI with
        member __.SetCursorPosition (x: int) (y: int) =
            __.x <- x
            __.y <- y

        member __.GetCursorPositionX (_: string) (x: int) = x

        member __.GetWindowWidth() = __.x
        member __.GetWindowHeight() = __.y
        member __.Write x y s =
            __.screen <- __.screen |>List.mapi (fun i ss ->
                match i with
                | ii when ii = y ->
                    ss.Substring(0, x) + s
                | _ -> ss)

    interface IDisposable with
        member __.Dispose() = ()

module ``Buff writeScreen`` =
    open System.Collections
    open System.Management.Automation

    [<Fact>]
    let ``should render top down.`` ()   =
        let rui = new MockRawUI()
        use buff = new Buff(rui,  (fun _ -> Seq.empty))

        let state: InternalState =
            { QueryState = { Query = "foo"; Cursor = 3; WindowBeginningX = 0; WindowWidth = rui.width }
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

        buff.writeTopDown state [] <| Ok []

        let expected =
            "query>foo                           cmatch and [0]"
            :: (generateLine rui.width (rui.height - 1))

        rui.screen
        |> shouldEqual expected

    [<Fact>]
    let ``should render bottom up.`` ()   =
        let rui = new MockRawUI()
        use buff = new Buff(rui, (fun _ -> Seq.empty))

        let state: InternalState =
            { QueryState = { Query = "hello*world*"; Cursor = 12; WindowBeginningX = 0; WindowWidth = rui.width }
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

        buff.writeBottomUp state [] <| Ok []

        let expected =
            "prompt>hello*world*                 notlike or [0]"
            :: (generateLine rui.width (rui.height - 1))
            |> List.rev

        rui.screen
        |> shouldEqual expected

    [<Fact>]
    let ``should render notification.`` ()   =
        let rui = new MockRawUI(80,30)
        use buff = new Buff(rui,  (fun _ -> Seq.empty))

        let state: InternalState =
            { QueryState = { Query = @"\"; Cursor = 1; WindowBeginningX = 0; WindowWidth = rui.width }
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

        let state = { state with Notification = pocof.PocofQuery.prepareNotification state }

        buff.writeTopDown state [] <| Ok []

        let expected =
            List.concat [ [ @"prompt>\                                                           match and [0]"
                            @"note>Invalid pattern '\' at offset 1. Illegal \ at end of pattern.              " ]
                          (generateLine rui.width (28)) ]

        rui.screen
        |> shouldEqual expected

    [<Fact>]
    let ``should render props notification.`` ()   =
        let rui = new MockRawUI(80,30)
        use buff = new Buff(rui,  (fun _ -> Seq.empty))

        let state: InternalState =
            { QueryState = { Query = @":unknown"; Cursor = 8; WindowBeginningX = 0; WindowWidth = rui.width }
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

        buff.writeTopDown state [] <| Error "Property not found"

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
        use buff = new Buff(rui, formatTableOutString)

        let state: InternalState =
            { QueryState = { Query = ""; Cursor = 0; WindowBeginningX = 0; WindowWidth = rui.width }
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

        let entries = [1..10] |> List.map (fun i ->
            DictionaryEntry("Number", i) |> Dict
        )

        buff.writeTopDown state entries <| Ok []

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
        use buff = new Buff(rui, formatTableOutString)

        let state: InternalState =
            { QueryState = { Query = ""; Cursor = 0; WindowBeginningX = 0; WindowWidth = rui.width }
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

        let entries = [1..100] |> List.map (fun i ->
            DictionaryEntry("Number", i) |> Dict
        )
        buff.writeTopDown state entries <| Ok []

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

    module ``display`` =
        [<Fact>]
        let ``should render head 30 of query when cursor 0.`` ()   =
            let rui = new MockRawUI(50,25)
            use buff = new Buff(rui, (fun _ -> Seq.empty))
            let query = [0..9] |> List.map (sprintf "%d-------->") |> String.concat ""
            let state: InternalState =
                { QueryState =
                      { Query = query
                        Cursor = 0
                        WindowBeginningX = 0
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

            buff.writeTopDown state [] <| Ok []

            let expected =
                "query>0-------->1-------->2--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

        [<Fact>]
        let ``should render head 30 of query when cursor 30.`` ()   =
            let rui = new MockRawUI(50,25)
            use buff = new Buff(rui,  (fun _ -> Seq.empty))
            let query = [0..9] |> List.map (sprintf "%d-------->") |> String.concat ""
            let state: InternalState =
                { QueryState =
                      { Query = query
                        Cursor = 0
                        WindowBeginningX = 0
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

            buff.writeTopDown state  [] <| Ok []

            let expected =
                "query>0-------->1-------->2--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

        [<Fact>]
        let ``should render mid 30 of query when cursor 45.`` ()   =
            let rui = new MockRawUI(50,25)
            use buff = new Buff(rui,  (fun _ -> Seq.empty))
            let query = [0..9] |> List.map (sprintf "%d-------->") |> String.concat ""
            let state: InternalState =
                { QueryState =
                      { Query = query
                        Cursor = 45
                        WindowBeginningX = 15
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

            buff.writeTopDown state  [] <| Ok []

            let expected =
                "query>---->2-------->3-------->4---- match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected

        [<Fact>]
        let ``should render tail 30 of query when cursor 90.`` ()   =
            let rui = new MockRawUI(50,25)
            use buff = new Buff(rui,  (fun _ -> Seq.empty))
            let query = [0..9] |> List.map (sprintf "%d-------->") |> String.concat ""
            let state: InternalState =
                { QueryState =
                      { Query = query
                        Cursor = 90
                        WindowBeginningX = 70
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

            buff.writeTopDown state  [] <| Ok []

            let expected =
                "query>7-------->8-------->9--------> match and [0]"
                :: (generateLine rui.width (rui.height - 1))

            rui.screen
            |> shouldEqual expected
