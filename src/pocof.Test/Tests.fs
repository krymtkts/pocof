module Tests

open Xunit
open FsUnitTyped

module ``Pocof Tests`` =

    [<Fact>]
    let ``Sample test should equals 1`` () = 1 |> shouldEqual 1

[<EntryPoint>]
let main argv = 0
