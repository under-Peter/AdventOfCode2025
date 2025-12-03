namespace Days

open System.IO
open System
open FSharpx

module Util =
    let parseInt64: string -> option<int64> =
        Int64.TryParse
        >> function
            | true, num -> Some num
            | _ -> None

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
        | (str: string) when String.length str > 1 -> str[0], Util.parseInt str[1..]
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

module Day2 =
    let rangeFromStrings strFrom strTo =
        match Util.parseInt64 strFrom, Util.parseInt64 strTo with
        | Some fromId, Some toId -> Some(fromId, toId)
        | _ -> failwith "Could not ids"

    let readRanges path =
        path
        |> File.ReadAllText
        |> fun line -> line.Split ','
        |> Array.map (fun range -> range.Split('-'))
        |> Array.choose (function
            | [| fromId; toId |] -> rangeFromStrings fromId toId
            | _ -> failwith "Could not parse range")

    let powersOf10 = 0L :: List.scan (fun state _ -> 10L * state) 1L [ 1..18 ]

    let numberOfDigits num =
        List.findIndexBack ((>=) num) powersOf10

    let splitNumber num =
        match numberOfDigits num with
        | nDigits when nDigits % 2 = 1 -> None
        | nDigits -> Some(num / powersOf10[nDigits / 2 + 1], num % powersOf10[nDigits / 2 + 1])

    let isValidIdPart1 =
        splitNumber >> Option.map (fun (x, y) -> x <> y) >> Option.defaultValue true

    let extractInvalidIdsInRange validityCheck (fromId, toId) =
        seq { fromId..toId } |> Seq.filter (not << validityCheck)

    let solveImpl1 path =
        path
        |> readRanges
        |> Seq.map (Seq.sum << extractInvalidIdsInRange isValidIdPart1)
        |> Seq.sum

    // reversed because it's easier
    let reverseDigitsOfNumber (number: int64) =
        List.unfold (fun (s: int64) -> if s = 0L then None else Some(s % 10L, s / 10L)) number

    let allPatterns (l: list<int64>) =
        seq {
            for i in [ 1 .. (l.Length / 2) ] do
                if l.Length % i = 0 then
                    Seq.reduce (@) (Seq.replicate (l.Length / i) (List.take i l))
        }

    let isValidIdPart2 num =
        num
        |> reverseDigitsOfNumber
        |> fun digits -> Seq.contains digits (allPatterns digits)
        |> not

    let solveImpl2 path =
        path
        |> readRanges
        |> Seq.map (Seq.sum << extractInvalidIdsInRange isValidIdPart2)
        |> Seq.sum

    let solve () =
        printfn $"""%A{solveImpl1 "../Days/data/day02.txt"} ref(52316131093L)"""
        printfn $"""%A{solveImpl2 "../Days/data/day02.txt"} ref(69564213293L)"""

module Day3 =
    let parseLine line =
        line
        |> String.toCharArray
        |> Array.map (Util.parseInt << string)
        |> fun xs ->
            match Array.choose id xs with
            | ys when ys.Length = xs.Length -> ys
            | _ -> failwithf "could not parse line '%s'" line

    let parseFile = File.ReadLines >> Seq.map parseLine

    let indEarliestMax (xs: array<int>) =
        seq { 0 .. xs.Length - 1 }
        |> Seq.fold (fun iMax i -> if xs[i] > xs[iMax] then i else iMax) 0


    let rec maxJoltageBatteries pick nums =
        match pick with
        | 0 -> []
        | _ ->
            let pickableBatteries = Array.take (Array.length nums - (pick - 1)) nums
            let ind = indEarliestMax pickableBatteries

            int64 pickableBatteries[ind]
            :: maxJoltageBatteries (pick - 1) (Array.skip (ind + 1) nums)

    let maxJoltage pick nums =
        nums |> maxJoltageBatteries pick |> List.reduce (fun a b -> 10L * a + b)


    let solveImpl1 = parseFile >> Seq.sumBy (maxJoltage 2)
    let solveImpl2 = parseFile >> Seq.sumBy (maxJoltage 12)

    let solve () =
        printfn $"""%A{solveImpl1 "../Days/data/day03_example.txt"} ref(357)"""
        printfn $"""%A{solveImpl1 "../Days/data/day03.txt"} ref(17281)"""
        printfn $"""%A{solveImpl2 "../Days/data/day03_example.txt"} ref(3121910778619)"""
        printfn $"""%A{solveImpl2 "../Days/data/day03.txt"} ref(171388730430281)"""
