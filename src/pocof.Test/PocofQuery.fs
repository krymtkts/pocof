module PocofQuery

open Xunit
open FsUnitTyped
open pocof

let state: PocofData.InternalState =
    { Query = ""
      QueryState =
        { Matcher = PocofData.Matcher.MATCH
          Operator = PocofData.Operator.OR
          CaseSensitive = false
          Invert = false }
      PropertySearch = PocofData.PropertySearch.NonSearch
      Notification = ""
      SuppressProperties = false }

module run =
    ()

module props =
    let entries = [ "Name"; "Attribute"; "Length" ]

    [<Fact>]
    let ``should returns OK with emtpy list.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.NonSearch } entries
        |> shouldEqual (Ok [])

    [<Fact>]
    let ``should returns Error with 'Property not found'.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.Search "No" } entries
        |> shouldEqual (Error "Property not found")

    [<Fact>]
    let ``should returns Ok with filtered properties.`` () =
        PocofQuery.props { state with PropertySearch = PocofData.PropertySearch.Search "Na" } entries
        |> shouldEqual (Ok [ "Name" ])
