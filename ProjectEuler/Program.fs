open System
open Problems

[<EntryPoint>]
let main argv =
    printfn "%s" (Problem4 ())
    Console.ReadKey() |> ignore
    0 
