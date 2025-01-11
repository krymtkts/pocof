namespace pocof.Inspector

open Inspector

module Program =
    [<EntryPoint>]
    let main _ =
        printMemoryLayout ()
        0
