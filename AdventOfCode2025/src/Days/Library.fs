namespace Days

open System.IO
open System

module Util =
    let flip f x y = f y x

    let parseInt: string -> option<int> =
        Int32.TryParse
        >> function
            | true, num -> Some num
            | _ -> None
    let difference (x,  y) = x - y

module Day1 =
    let parseLine (line: string) =
        line.Split ' '
        |> Array.choose Util.parseInt
        |> function
            | [| elem1; elem2 |] -> Some(elem1, elem2)
            | _ -> None

    // parseFile to two lists of the values in the file *in reverse order*
    let parseFile =
        let accumulateTwoReversedLists =
            fun (accX, accY) line ->
                line
                |> parseLine
                |> function
                    | Some(x, y) -> x :: accX, y :: accY
                    | None -> accX, accY

        File.ReadLines >> Seq.fold accumulateTwoReversedLists ([], [])

    let solveImpl1 =
        let absDiff = Util.difference >> abs;

        parseFile
        >> fun (xs, ys) -> List.zip (List.sort xs) (List.sort ys)
        >> List.sumBy absDiff

    let solveImpl2 =
        parseFile
        >> fun (xs, ys) ->
            let ysFreqMap = ys |> List.countBy id |> Map.ofList

            let weightedProduct (x, xCountInX) =
                let xCountInY = Map.tryFind x ysFreqMap |> Option.defaultValue 0
                x * xCountInX * xCountInY

            xs |> List.countBy id |> List.sumBy weightedProduct

    let solve () =
        let result1 = solveImpl1 "../Days/data/day01_1.txt"
        let result2 = solveImpl2 "../Days/data/day01_1.txt"
        printfn "%d ref(3574690)" result1
        printfn "%d ref(22565391)" result2

module Day2 =
    let parseLine (line: string) =
        line.Split ' ' |> Array.choose Util.parseInt

    let parseFile = File.ReadLines >> Seq.map parseLine

    let boolToInt =
        function
        | true -> 1
        | false -> 0

    let isSafe =
        let accumulateProperties =
            fun (minVal, maxVal, zeroCount) newVal ->
                min minVal newVal, max maxVal newVal, zeroCount + boolToInt (newVal = 0)

        Array.pairwise
        >> Array.map Util.difference
        >> Array.fold accumulateProperties (0, 0, 0)
        >> function
            | minDiff, 0, 0 when List.contains minDiff [-3;-2;-1] -> true
            | 0, maxDiff, 0 when List.contains maxDiff [1;2;3] -> true
            | _ -> false

    let solveImpl1 = parseFile >> Seq.sumBy (isSafe >> boolToInt)

    let solveImpl2 =
        let upToOneLevelMissing (levels: array<int>) =
            let inds = seq { 0 .. (levels.Length - 1) }

            seq {
                yield levels
                yield! Seq.map (Util.flip Array.removeAt levels) inds
            }

        let isAnyVersionSafe = upToOneLevelMissing >> Seq.exists isSafe

        parseFile >> Seq.sumBy (isAnyVersionSafe >> boolToInt)

    let solve () =
        let result1 = solveImpl1 "../Days/data/day02_1.txt"
        let result2 = solveImpl2 "../Days/data/day02_1.txt"
        printfn "%d ref(269)" result1
        printfn "%d ref(337)" result2
