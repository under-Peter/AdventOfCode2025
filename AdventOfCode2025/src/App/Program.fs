// For more information see https://aka.ms/fsharp-console-apps
open Days
open System

// Dispatch table: map numbers to functions
let dispatch n =
    match n with
    | 1 -> Day1.solve ()
    | 2 -> Day2.solve ()
    | _ -> failwithf "Unknown function id: %d." n

[<EntryPoint>]
let main (argv: string[]) =
    match argv with
    | [| arg |] ->
        match Int32.TryParse arg with
        | true, n -> dispatch n
        | _ -> failwith "provide day"
    | _ -> failwith "provide day"

    0
