module PocofTest.Query

open System.Collections
open System.Management.Automation

open Xunit
open FsUnitTyped

open Expecto
open Expecto.Flip

open Pocof

let initState () : Data.InternalState =
    { QueryState =
        { Query = ""
          Cursor = 0
          WindowBeginningCursor = 0
          WindowWidth = 0
          InputMode = Data.InputMode.Input }
      QueryCondition =
        { Matcher = Data.Matcher.Match
          Operator = Data.Operator.Or
          CaseSensitive = false
          Invert = false }
      PropertySearch = Data.PropertySearch.NoSearch
      SuppressProperties = false
      Refresh = Data.Refresh.Required }

let state = initState ()
let properties = [ "Name"; "Attribute"; "Length" ]

let caseSensitive (s: Data.InternalState) =
    { s with
        QueryCondition.CaseSensitive = true }

[<Tests>]
let tests_DictionaryEntry =
    testList
        "DictionaryEntry"
        [

          test "When key is 'Key'" {
              let d = DictionaryEntry("Jane", "Doe")
              d["Key"] |> Expect.equal "should return key" "Jane"
          }

          test "When key is 'Value'" {
              let d = DictionaryEntry("Jane", "Doe")
              d["Value"] |> Expect.equal "should return value" "Doe"
          }

          test "When key is not found" {
              let d = DictionaryEntry("Jane", "Doe")
              d["Ke"] |> Expect.equal "should return null for not found key" null
          }

          ]

[<Tests>]
let tests_PSObject =
    testList
        "PSObject"
        [

          test "When property is 'Length'" {
              let o = "a" |> PSObject.AsPSObject
              o["Length"] |> Expect.equal "should return 1 for Length" 1
          }
          test "When property is not found" {
              let o = "a" |> PSObject.AsPSObject
              o["Lengt"] |> Expect.equal "should return null for not found property" null
          }

          ]

[<Tests>]
let tests_props =
    testList
        "props"
        [

          test "When PropertySearch is NoSearch" {
              Query.props
                  properties
                  { state with
                      PropertySearch = Data.PropertySearch.NoSearch }
              |> Expect.equal "should return Ok with empty list" (Ok [])
          }

          test "When PropertySearch is Search 'No' (not found)" {
              Query.props
                  properties
                  { state with
                      PropertySearch = Data.PropertySearch.Search "No" }
              |> Expect.equal "should return Error with 'Property not found'" (Error "Property not found")
          }

          test "When PropertySearch is Search '' (empty string)" {
              Query.props
                  properties
                  { state with
                      PropertySearch = Data.PropertySearch.Search "" }
              |> Expect.equal "should return Ok with all properties" (Ok [ "Name"; "Attribute"; "Length" ])
          }

          test "When PropertySearch is Search 'Na' (filtered)" {
              Query.props
                  properties
                  { state with
                      PropertySearch = Data.PropertySearch.Search "Na" }
              |> Expect.equal "should return Ok with filtered properties" (Ok [ "Name" ])
          }

          test "When PropertySearch is Search 'na' (case-insensitive)" {
              Query.props
                  properties
                  { state with
                      PropertySearch = Data.PropertySearch.Search "na" }
              |> Expect.equal "should return Ok with filtered properties (case-insensitive)" (Ok [ "Name" ])
          }

          test "When PropertySearch is Rotate with empty string" {
              Query.props
                  properties
                  { state with
                      PropertySearch = Data.PropertySearch.Rotate("", [ "Name" ]) }
              |> Expect.equal "should return Ok with all properties (rotate)" (Ok [ "Name"; "Attribute"; "Length" ])
          }

          test "When PropertySearch is Rotate with filter 'Na'" {
              Query.props
                  properties
                  { state with
                      PropertySearch = Data.PropertySearch.Rotate("Na", [ "Name" ]) }
              |> Expect.equal "should return Ok with filtered properties (rotate)" (Ok [ "Name" ])
          }

          ]

module run =
    open System.Collections

    let duplicateCase (s: string) = [ s; s.ToLower() ]
    let mapToObj = List.map (PSObject.AsPSObject >> Data.Entry.Obj)

    let genList s =
        List.collect duplicateCase s |> mapToObj

    let matcher m (s: Data.InternalState) = { s with QueryCondition.Matcher = m }
    let query q (s: Data.InternalState) = { s with QueryState.Query = q }
    let invert (s: Data.InternalState) = { s with QueryCondition.Invert = true }

    let opAnd (s: Data.InternalState) =
        { s with
            QueryCondition.Operator = Data.Operator.And }

    [<Tests>]
    let ``tests_run: with a simple query`` =
        let entries = genList [ "Name"; "Attribute"; "Length" ] |> PSeq.ofSeq
        let props = Map [ ("length", "Length") ]

        testList
            "run: with a simple query"
            [

              test "When entry list is empty" {
                  let context, _ = Query.prepare state

                  Query.run context PSeq.empty props
                  |> List.ofSeq
                  |> Expect.equal "should return empty" []
              }

              testList
                  "of MATCH"
                  [

                    let state = state |> matcher Data.Matcher.Match |> query "a"

                    test "When query is empty" {
                        let state = initState () |> matcher Data.Matcher.Match
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return all entries" (entries |> List.ofSeq)
                    }

                    test "When query is invalid pattern" {
                        let state = state |> query "+"
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return all entries" (entries |> List.ofSeq)
                    }

                    test "When query is 'a' (filtered)" {
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return filtered entries" (genList [ "Name"; "Attribute" ])
                    }

                    test "When matcher is match and case sensitive" {
                        let state = caseSensitive state
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal
                            "should return filtered entries (case sensitive)"
                            (mapToObj [ "Name"; "name"; "attribute" ])
                    }

                    test "When matcher is match and invert result" {
                        let state = invert state
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return filtered entries (invert)" (genList [ "Length" ])
                    }

                    test "When composite query with or operator" {
                        let state = state |> query "a N"
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return all entries (or)" (entries |> List.ofSeq)
                    }

                    test "When composite query with and operator" {
                        let state = state |> query "a N" |> opAnd
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return filtered entries (and)" (genList [ "Name" ])
                    } ]

              testList
                  "of LIKE"
                  [

                    let state = state |> matcher Data.Matcher.Like |> query "a*"

                    test "When query is empty" {
                        let state = initState () |> matcher Data.Matcher.Like
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return all entries" (entries |> List.ofSeq)
                    }

                    test "When matcher is like" {
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return matched entries" (genList [ "Attribute" ])
                    }

                    test "When matcher is like and case sensitive" {
                        let state = caseSensitive state
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return matched entries (case sensitive)" (mapToObj [ "attribute" ])
                    }

                    test "When matcher is like and invert result" {
                        let state = invert state
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return filtered entries (invert)" (genList [ "Name"; "Length" ])
                    }

                    test "When composite query with or operator" {
                        let state = state |> query "*e* N*"
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return all entries (or)" (entries |> List.ofSeq)
                    }

                    test "When composite query with and operator" {
                        let state = state |> query "*e* N*" |> opAnd
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return filtered entries (and)" (genList [ "Name" ])
                    } ]

              testList
                  "of EQ"
                  [

                    let state = state |> matcher Data.Matcher.Eq |> query "Name"

                    test "When query is empty" {
                        let state = initState () |> matcher Data.Matcher.Eq
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return all entries" (entries |> List.ofSeq)
                    }

                    test "When matcher is eq" {
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return matched entries" (genList [ "Name" ])
                    }

                    test "When matcher is eq and case sensitive" {
                        let state = caseSensitive state
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return matched entries (case sensitive)" (mapToObj [ "Name" ])
                    }

                    test "When matcher is eq and invert result" {
                        let state = invert state
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return filtered entries (invert)" (genList [ "Attribute"; "Length" ])
                    }

                    test "When composite query with or operator" {
                        let state = state |> query "Name Length"
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return filtered entries (or)" (genList [ "Name"; "Length" ])
                    }

                    test "When composite query with and operator" {
                        let state = state |> query "Name Length" |> opAnd
                        let context, _ = Query.prepare state

                        Query.run context entries props
                        |> List.ofSeq
                        |> Expect.equal "should return filtered entries (and)" []
                    }

                    ]

              ]


    [<Tests>]
    let ``tests with a Dictionary query`` =
        let props = Map []
        let mapToDict = List.map Data.Entry.Dict

        let entries =
            mapToDict
                [ DictionaryEntry("John", "Doe")
                  DictionaryEntry("Jane", "Doe")
                  DictionaryEntry("Ming", "Wu")
                  DictionaryEntry("Taro", "Nanashi") ]
            |> PSeq.ofSeq

        testList
            "with a Dictionary query"
            [

              test "When composite query with or operator" {
                  let state = state |> query "e"
                  let context, _ = Query.prepare state

                  Query.run context entries props
                  |> List.ofSeq
                  |> Expect.equal
                      "should return filtered entries (or)"
                      (mapToDict [ DictionaryEntry("John", "Doe"); DictionaryEntry("Jane", "Doe") ])
              }

              test "When composite query with and operator" {
                  let state = state |> query "ne" |> opAnd
                  let context, _ = Query.prepare state

                  Query.run context entries props
                  |> List.ofSeq
                  |> Expect.equal "should return filtered entries (and)" (mapToDict [ DictionaryEntry("Jane", "Doe") ])
              }

              test "When property query" {
                  let props =
                      DictionaryEntry("Jane", "Doe")
                      |> PSObject.AsPSObject
                      |> _.Properties
                      |> Seq.map (fun p -> p.Name.ToLower(), p.Name)
                      |> Map

                  let state = state |> query ":key  ja" |> opAnd
                  let context, _ = Query.prepare state

                  Query.run context entries props
                  |> List.ofSeq
                  |> Expect.equal
                      "should return filtered entries (property query)"
                      (mapToDict [ DictionaryEntry("Jane", "Doe") ])
              }

              test "When querying a non-existing property (should return all)" {
                  let props =
                      DictionaryEntry("Jane", "Doe")
                      |> PSObject.AsPSObject
                      |> _.Properties
                      |> Seq.map (fun p -> p.Name.ToLower(), p.Name)
                      |> Map

                  let state = state |> query ":title  ja" |> opAnd
                  let context, _ = Query.prepare state

                  Query.run context entries props
                  |> List.ofSeq
                  |> Expect.equal "should return all entries (non-existing property)" (entries |> List.ofSeq)
              }

              test "When querying a non-existing property (should return empty)" {
                  let props =
                      DictionaryEntry("Jane", "Doe")
                      |> PSObject.AsPSObject
                      |> _.Properties
                      |> Seq.map (fun p -> p.Name.ToLower(), p.Name)
                      |> Map

                  let state = state |> query ":key ja" |> opAnd

                  let entries =
                      [ "d" ] |> List.map (PSObject.AsPSObject >> Data.Entry.Obj) |> PSeq.ofSeq

                  let context, _ = Query.prepare state

                  Query.run context entries props
                  |> List.ofSeq
                  |> Expect.equal "should return empty" []
              }

              ]

    [<Tests>]
    let ``tests with a Property query`` =
        let getPsObj (f: string, l: string) =
            let ret = PSObject()

            [ "Fn", f; "Ln", l ]
            |> List.iter (fun (k, v) -> ret.Properties.Add(PSNoteProperty(k, v)))

            ret

        let mapToPsObj = List.map (getPsObj >> Data.Entry.Obj)

        let entries =
            mapToPsObj [ ("John", "Doe"); ("Jane", "Doe"); ("Ming", "Wu"); ("Taro", "Nanashi") ]

        let props = Map [ "fn", "Fn"; "ln", "Ln" ]

        testList
            "with a Property query"
            [

              test "When composite query with or operator" {
                  let state = state |> query ":fn a  :ln  d"
                  let context, _ = Query.prepare state
                  let filtered = [ entries.[0]; entries.[1]; entries.[3] ]

                  Query.run context (entries |> PSeq.ofSeq) props
                  |> List.ofSeq
                  |> Expect.equal "should return filtered entries (or)" filtered
              }

              test "When composite query with and operator" {
                  let state = state |> query ":fn a :ln d" |> opAnd
                  let context, _ = Query.prepare state
                  let filtered = [ entries.[1] ]

                  Query.run context (entries |> PSeq.ofSeq) props
                  |> List.ofSeq
                  |> Expect.equal "should return filtered entries (and)" filtered
              }

              test "When property not exists" {
                  let state = state |> query ":f a"
                  let context, _ = Query.prepare state

                  Query.run context (entries |> PSeq.ofSeq) props
                  |> List.ofSeq
                  |> Expect.equal "should return all entries (property not exists)" entries
              }

              test "When incomplete composite query" {
                  let state = state |> query ":fn "
                  let context, _ = Query.prepare state

                  Query.run context (entries |> PSeq.ofSeq) props
                  |> List.ofSeq
                  |> Expect.equal "should return all entries (incomplete composite query)" entries
              }

              test "When incomplete composite query with filter" {
                  let state = state |> query "a :fn "
                  let context, _ = Query.prepare state
                  let filtered = [ entries.[1]; entries.[3] ]

                  Query.run context (entries |> PSeq.ofSeq) props
                  |> List.ofSeq
                  |> Expect.equal "should return filtered entries (incomplete composite query with filter)" filtered
              }

              test "When a non-existent property query exists after a correct query" {
                  let state = state |> query "a :f  e "
                  let context, _ = Query.prepare state
                  let filtered = [ entries.[1]; entries.[3] ]

                  Query.run context (entries |> PSeq.ofSeq) props
                  |> List.ofSeq
                  |> Expect.equal "should return filtered entries (non-existent property after correct query)" filtered
              }

              test "When a non-existent property query exists before a correct query" {
                  let state = state |> query ":f e a"
                  let context, _ = Query.prepare state
                  let filtered = [ entries.[1]; entries.[3] ]

                  Query.run context (entries |> PSeq.ofSeq) props
                  |> List.ofSeq
                  |> Expect.equal "should return filtered entries (non-existent property before correct query)" filtered
              }

              ]

    open System
    open System.Globalization

    [<Tests>]
    let ``tests with a locale`` =
        let entries =
            [ DateTime.Parse("2024-01-01")
              DateTime.Parse("2024-01-02")
              DateTime.Parse("2024-01-03")
              DateTime.Parse("2024-01-04") ]
            |> List.map (PSObject.AsPSObject >> Data.Entry.Obj)

        testList
            "with a locale"
            [

              test "When composite query with or operator and locale is en-US" {
                  let culture = Threading.Thread.CurrentThread.CurrentCulture
                  let testCulture = CultureInfo.GetCultureInfo("en-US").Clone() :?> CultureInfo
                  testCulture.DateTimeFormat.ShortDatePattern <- "yyyy-MM-dd"
                  Threading.Thread.CurrentThread.CurrentCulture <- testCulture

                  let state = state |> query "01-04"
                  let context, _ = Query.prepare state
                  let filtered = [ entries |> List.last ]

                  Query.run context (entries |> PSeq.ofSeq) (Map [])
                  |> List.ofSeq
                  |> Expect.equal "should return filtered entries (locale en-US)" filtered

                  Threading.Thread.CurrentThread.CurrentCulture <- culture
              }

              ]
