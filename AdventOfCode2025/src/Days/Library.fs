namespace Days

open System.IO
open System

module Util =
    let parseInt: string -> option<int> =
        Int32.TryParse
        >> function
            | true, num -> Some num
            | _ -> None

    let boolToInt =
        function
        | true -> 1
        | false -> 0

module Day1 =
    let wrapAround = 100
    let startPos = 50

    let parseLine =
        function
        | (str: string) when str.Length > 1 -> str[0], Util.parseInt str[1..]
        | _ -> ' ', None
        >> function
            | 'L', Some steps -> Some -steps
            | 'R', Some steps -> Some steps
            | _ -> None

    let wrapAroundPosition x =
        let rem = x % wrapAround
        rem + if rem < 0 then wrapAround else 0

    let parseFile = File.ReadLines >> Seq.choose parseLine

    let moveWithWrapAround pos steps = pos + steps |> wrapAroundPosition

    let countZerosVisited =
        Seq.scan moveWithWrapAround startPos >> Seq.sumBy ((=) 0 >> Util.boolToInt)

    let solveImpl1 = parseFile >> countZerosVisited

    let expandToSingleSteps = Seq.collect (fun i -> Seq.replicate (abs i) (sign i))
    let solveImpl2 = parseFile >> expandToSingleSteps >> countZerosVisited

    let solveImpl2' =
        let isCrossing posBeforeWrap =
            posBeforeWrap < 0 || posBeforeWrap > wrapAround

        let countCrossings (pos, step) =
            let nCycles = abs step / wrapAround
            // count 0-positions but don't count arriving at a 0 in the current step
            match pos, step with
            | 0, step -> 1 + nCycles - Util.boolToInt (step % wrapAround = 0)
            | pos, step -> nCycles + Util.boolToInt (isCrossing (pos + step % wrapAround))

        parseFile
        >> Seq.toList
        >> (fun steps -> List.zip (List.scan moveWithWrapAround startPos steps) (List.append steps (List.singleton 0)))
        >> List.sumBy countCrossings


    let solve () =
        printfn $"""%d{solveImpl1 "../Days/data/day01.txt"} ref(1105)"""
        printfn $"""%d{solveImpl2 "../Days/data/day01.txt"} ref(6599)"""
        printfn $"""%d{solveImpl2' "../Days/data/day01.txt"} ref(6599)"""
