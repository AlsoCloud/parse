module Run

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    System.Console.ReadLine () |> printfn "from pipe: %A"
    0 // return an integer exit code

