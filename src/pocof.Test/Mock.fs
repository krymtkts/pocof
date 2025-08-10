namespace Pocof.Test

open System

open Pocof.LanguageExtension
open Pocof.Screen

[<AutoOpen>]
module Mock =
    let generateLine x y =
        List.replicate y <| String.replicate x " "

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
                        | ii when ii = y -> ss |> String.upToIndex x |> (+) s
                        | _ ->
                            let l = (__ :> IRawUI).GetLengthInBufferCells ss

                            match l <= __.width with
                            | true -> ss + String.replicate (__.width - l) " "
                            | _ -> ss)

            member __.WriteLine() =
                (__ :> IRawUI).Write __.x __.y "\n"
                __.y <- __.y + 1

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
