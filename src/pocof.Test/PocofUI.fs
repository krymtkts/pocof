module PocofUI

open Xunit
open FsUnitTyped
open System
open pocof.PocofData
open pocof.PocofScreen

let generateLine x y = List.replicate y <| String.replicate x " "
type MockRawUI =
    val caAsInput: bool
    val mutable x: int
    val mutable y: int
    val mutable screen: string list

    static xx = 50
    static yy = 30
    new() =
        { caAsInput = Console.TreatControlCAsInput
          x = MockRawUI.xx
          y = MockRawUI.yy
          screen = generateLine  MockRawUI.xx MockRawUI.yy
          }

    interface IRawUI with
        member __.SetCursorPosition (x: int) (y: int) =
            __.x <- x
            __.y <- y

        member __.GetCursorPositionX (_: string) (x: int) = x

        member __.GetWindowWidth() = 50
        member __.GetWindowHeight() = 30
        member __.Write x y s =
            __.screen <- __.screen |>List.mapi (fun i ss ->
                match i with
                | ii when ii = y ->
                    ss.Substring(0, x) + s
                | _ -> ss)

    interface IDisposable with
        member __.Dispose() = ()

module ``Buff writeScreen`` =
    [<Fact>]
    let ``should render top down.`` ()   =
        let rui = new MockRawUI()
        let buff = new Buff(rui, "query", (fun _ -> Seq.empty))

        let state: InternalState =
            { Query = "foo"
              QueryState =
                { Matcher = MATCH
                  Operator = AND
                  CaseSensitive = true
                  Invert = false }
              PropertySearch = NonSearch
              Notification = ""
              SuppressProperties = false }

        buff.writeTopDown state 0 [] <| Ok []

        let expected =
            "query>foo                           cmatch and [0]" :: (generateLine MockRawUI.xx (MockRawUI.yy - 1))
        rui.screen
        |> shouldEqual expected

    [<Fact>]
    let ``should render bottom up.`` ()   =
        let rui = new MockRawUI()
        let buff = new Buff(rui, "prompt", (fun _ -> Seq.empty))

        let state: InternalState =
            { Query = "hello*world*"
              QueryState =
                { Matcher = LIKE
                  Operator = OR
                  CaseSensitive = false
                  Invert = true }
              PropertySearch = NonSearch
              Notification = ""
              SuppressProperties = false }

        buff.writeBottomUp state 0 [] <| Ok []

        let expected =
            "prompt>hello*world*                 notlike or [0]" :: (generateLine MockRawUI.xx (MockRawUI.yy - 1))
            |> List.rev
        rui.screen
        |> shouldEqual expected

    // TODO: test notification rendering.
    // TODO: test entries rendering.
