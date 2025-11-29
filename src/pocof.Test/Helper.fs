module PocofTest.Helper

open Expecto.Flip

open Pocof
open Pocof.Data

let expectInternalStateEqual message expected actual =
    let msg suffix = $"{message}: {suffix}"

    actual.QueryState |> Expect.equal (msg "correct QueryState") expected.QueryState

    actual.QueryCondition
    |> Expect.equal (msg "correct QueryCondition") expected.QueryCondition

    actual.PropertySearch
    |> Expect.equal (msg "correct PropertySearch") expected.PropertySearch

    actual.SuppressProperties
    |> Expect.equal (msg "correct SuppressProperties") expected.SuppressProperties

    actual.Refresh |> Expect.equal (msg "correct Refresh") expected.Refresh

    match expected.QueryCache, actual.QueryCache with
    | ValueSome expectedCache, ValueSome actualCache ->
        actualCache.Key |> Expect.equal (msg "correct QueryCache.Key") expectedCache.Key
    // TODO: check Queries.
    | ValueNone, ValueNone -> ()
    | _ -> failwith (msg "mismatched QueryCache state between expected and actual. ")
