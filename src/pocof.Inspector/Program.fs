namespace pocof.Inspector

open Inspector

module Program =
    [<EntryPoint>]
    let main argv =
        printMemoryLayout argv
        0
