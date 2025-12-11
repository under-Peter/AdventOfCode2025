namespace Days

open System.IO
open System.Collections.Generic
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

    let flip f x y = f y x

    // duplicate keys will be taken from first
    let mergeMaps m1 m2 = Map.foldBack Map.add m1 m2

    let timed foo =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let res = foo ()
        stopWatch.Stop()
        stopWatch.Elapsed.TotalMilliseconds, res

    let printTimedResult foo ref =
        timed foo |> fun (t, res) -> printfn "%A %A (%A ms)" res (res = ref) t


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
        |> Array.map (string >> Util.parseInt)
        |> fun xs ->
            match Array.choose id xs with
            | ys when ys.Length = xs.Length -> ys
            | _ -> failwithf "could not parse line '%s'" line

    let parseFile = File.ReadLines >> Seq.map parseLine

    let indEarliestMax xs =
        seq { 0 .. Array.length xs - 1 }
        |> Seq.fold (fun iMax i -> if xs[i] > xs[iMax] then i else iMax) 0

    let rec maxJoltageBatteries pick (nums: int[]) =
        match pick with
        | 0 -> []
        | _ ->
            let pickableBatteries = nums[0 .. Array.length nums - pick]
            let ind = indEarliestMax pickableBatteries

            int64 pickableBatteries[ind] :: maxJoltageBatteries (pick - 1) nums[ind + 1 ..] //(Array.skip (ind + 1) nums)

    let maxJoltage pick nums =
        nums |> maxJoltageBatteries pick |> List.reduce (fun a b -> 10L * a + b)


    let solveImpl1 = parseFile >> Seq.sumBy (maxJoltage 2)
    let solveImpl2 = parseFile >> Seq.sumBy (maxJoltage 12)

    let solve () =
        printfn $"""%A{solveImpl1 "../Days/data/day03_example.txt"} ref(357)"""
        printfn $"""%A{solveImpl1 "../Days/data/day03.txt"} ref(17281)"""
        printfn $"""%A{solveImpl2 "../Days/data/day03_example.txt"} ref(3121910778619)"""
        printfn $"""%A{solveImpl2 "../Days/data/day03.txt"} ref(171388730430281)"""

module Day4 =
    let parseFile =
        File.ReadLines
        >> Seq.indexed
        >> Seq.collect (fun (yCoord, line) ->
            line
            |> Seq.indexed
            |> Seq.map (fun (xCoord, symbol) -> symbol, (xCoord, yCoord)))

    let rollCoordinates = Seq.filter (fst >> (=) '@') >> Seq.map snd >> set

    let neighborOffsets =
        Array.allPairs [| -1 .. 1 |] [| -1 .. 1 |] |> Array.filter ((<>) (0, 0))

    let neighbors (x, y) =
        neighborOffsets |> Array.map (fun (dx, dy) -> x + dx, y + dy)

    let movableRolls rollCoordinates =
        rollCoordinates
        |> Set.filter (neighbors >> Seq.filter rollCoordinates.Contains >> Seq.length >> (>) 4)

    let solveImpl1 = parseFile >> rollCoordinates >> movableRolls >> Set.count

    let solveImpl2 =
        parseFile
        >> rollCoordinates
        >> Seq.unfold (fun rolls ->
            match movableRolls rolls with
            | toRemove when toRemove.IsEmpty -> None
            | toRemove -> Some(toRemove.Count, rolls - toRemove))
        >> Seq.sum


    let solve () =
        printfn $"""%A{solveImpl1 "../Days/data/day04_example.txt"} ref(13)"""
        printfn $"""%A{solveImpl1 "../Days/data/day04.txt"} ref(1351)"""
        printfn $"""%A{solveImpl2 "../Days/data/day04_example.txt"} ref(43)"""
        printfn $"""%A{solveImpl2 "../Days/data/day04.txt"} ref(8345)"""

module Day5 =

    let parseIngredientId = Util.parseInt64

    let parseFreshIdRange =
        String.splitChar [| '-' |]
        >> function
            | [| strIdFrom; strIdTo |] ->
                match parseIngredientId strIdFrom, parseIngredientId strIdTo with
                | Some idFrom, Some idTo -> Some(idFrom, idTo)
                | _ -> None
            | _ -> None


    let parseFile =
        File.ReadLines
        >> Seq.map (fun line -> parseFreshIdRange line, parseIngredientId line)
        >> Seq.toArray
        >> Array.unzip
        >> fun (ranges, ids) -> Array.choose id ranges, Array.choose id ids


    let unionOfDisjoint =
        let sortedRangesOverlap (_, hi) (lo, _) = lo <= hi
        let combineSortedRanges (lo, hi1) (_, hi2) = lo, max hi1 hi2

        Array.sortBy fst
        >> Array.fold
            (fun compressed range ->
                match compressed with
                | head :: tail when sortedRangesOverlap head range -> combineSortedRanges head range :: tail
                | _ -> range :: compressed)
            []
        >> List.toArray
        >> Array.rev


    // assumes ranges is a sorted (by fst) array of disjoint intervals
    let isFresh ranges =
        let rec binarySearch x ranges =
            match ranges with
            | [||] -> None
            | _ ->
                let mid = ranges.Length / 2

                match ranges[mid] with
                | _, h when h < x -> binarySearch x ranges[mid + 1 ..]
                | l, _ when x < l -> binarySearch x ranges[.. mid - 1]
                | range -> Some range

        (fun id -> binarySearch id ranges) >> Option.isSome

    let countFresh (ranges, ids) =
        let compressed = unionOfDisjoint ranges
        ids |> Array.sumBy (isFresh compressed >> Util.boolToInt)

    let solveImpl1 = parseFile >> countFresh

    let solveImpl2 =
        parseFile
        >> fst
        >> unionOfDisjoint
        >> Array.sumBy (fun (lo, hi) -> hi - lo + 1L)


    let solve () =
        printfn $"""%A{solveImpl1 "../Days/data/day05_example.txt"} ref(3)"""
        printfn $"""%A{solveImpl1 "../Days/data/day05.txt"} ref(726)"""
        printfn $"""%A{solveImpl2 "../Days/data/day05_example.txt"} ref(14)"""
        printfn $"""%A{solveImpl2 "../Days/data/day05.txt"} ref(354226555270043)"""

module Day6 =
    let parseLine (line: string) =
        line.Split ' ' |> Array.filter (String.length >> (<) 0)

    let translateOps =
        Array.map (function
            | "+" -> (+)
            | "*" -> (*)
            | x -> failwithf "could not parse operator %s" x)

    let translateOperands = Array.map (Array.choose Util.parseInt64)


    let parseFilePart1 =
        File.ReadAllLines
        >> Array.map parseLine
        >> fun lines ->
            let ops = translateOps lines[lines.Length - 1]

            let operands = lines[.. lines.Length - 2] |> translateOperands |> Array.transpose
            Array.zip ops operands

    let reduceOpOverArgs (op, args) = Array.reduce op args

    let solveImpl1 = parseFilePart1 >> Array.sumBy reduceOpOverArgs

    let parseFilePart2 path =
        let lines = File.ReadAllLines path
        let ops = translateOps (parseLine lines[lines.Length - 1]) |> Array.toList

        let charArray = lines[.. (lines.Length - 2)] |> Array.map String.toCharArray

        let splitAtNones =
            Array.fold
                (fun acc x ->
                    match x, acc with
                    | None, _ -> [] :: acc // start new list
                    | Some i, currentList :: rest -> (i :: currentList) :: rest
                    | _ -> failwith "unexpected failure")
                [ [] ]
            >> List.map List.toArray

        let operands =
            charArray
            |> Array.transpose // look at columns
            |> Array.map (System.String >> Util.parseInt64)
            |> splitAtNones
            |> List.rev

        List.zip ops operands

    let solveImpl2 = parseFilePart2 >> List.sumBy reduceOpOverArgs


    let solve () =
        printfn $"""%A{solveImpl1 "../Days/data/day06_example.txt"} ref(4277556)"""
        printfn $"""%A{solveImpl1 "../Days/data/day06.txt"} ref(7098065460541L)"""
        printfn $"""%A{solveImpl2 "../Days/data/day06_example.txt"} ref(3263827)"""
        printfn $"""%A{solveImpl2 "../Days/data/day06.txt"} ref(13807151830618)"""

module Day7 =
    let parseLine =
        String.toCharArray >> Seq.indexed >> Seq.filter (snd >> (<>) '.') >> Seq.toArray

    let parseFile path =
        let lines = path |> File.ReadLines |> Seq.map parseLine

        let source =
            lines
            |> Seq.head
            |> function
                | [| sourcePos, 'S' |] -> sourcePos // |> Array.head |> fst
                | l -> failwithf "could not use %A for first line" l

        let splitters =
            lines
            |> Seq.skip 1
            |> Seq.map (Array.filter (snd >> (=) '^') >> Array.map fst >> Set)

        source, splitters

    let newSplitBeams beams splitters =
        beams
        |> Set.intersect splitters
        |> Seq.map (fun x -> Set.ofArray [| x - 1; x + 1 |])
        |> Set.unionMany
        |> flip (-) splitters


    let solveImpl1 path =
        let source, splitters = parseFile path

        let splitBeams beams splitters =
            let intersection = Set.intersect beams splitters

            let newBeams = newSplitBeams beams splitters

            let unaffectedBeams = beams - intersection
            unaffectedBeams + newBeams, intersection |> Set.count

        Seq.fold
            (fun (beams: Set<int>, nSplits) splitters ->
                let newBeams, nNewSplits = splitBeams beams splitters
                newBeams, nSplits + nNewSplits)
            (Set.singleton source, 0)
            splitters
        |> snd

    let solveImpl2 path =
        let source, splitters = parseFile path

        let splitBeams beamsPreviousRow splittersCurrentRow pathDegeneracy =
            let intersection = Set.intersect beamsPreviousRow splittersCurrentRow

            let newBeams = newSplitBeams beamsPreviousRow splittersCurrentRow

            let unaffectedBeams = beamsPreviousRow - intersection

            let degeneracyBeforeIfSplit =
                Option.someIf (flip Set.contains splittersCurrentRow)
                >> Option.bind (flip Map.tryFind pathDegeneracy)
                >> Option.getOrElse 0L

            let degenNewBeams =
                newBeams
                |> Set.toArray
                |> Array.map (fun i ->
                    let degenUp = Map.tryFind i pathDegeneracy |> Option.getOrElse 0L
                    let degenUpLeft = degeneracyBeforeIfSplit (i - 1)
                    let degenUpRight = degeneracyBeforeIfSplit (i + 1)
                    i, degenUp + degenUpLeft + degenUpRight)
                |> Map

            let newDegeneracy =
                pathDegeneracy
                |> Map.filter (fun k _ -> Set.contains k intersection |> not)
                |> Util.mergeMaps degenNewBeams

            unaffectedBeams + newBeams, newDegeneracy

        Seq.fold
            (fun (beams, pathDegeneracy) splitters -> splitBeams beams splitters pathDegeneracy)
            (Set.singleton source, Map.add source 1L Map.empty)
            splitters
        |> snd
        |> Map.values // degeneracies
        |> Seq.sum

    let solveImpl2' path =
        let source, splittersSeq = parseFile path
        let splitters = Seq.toList splittersSeq

        let memo = new Dictionary<Tuple<int, list<Set<int>>>, int64>()

        let rec countPaths s splitters =
            let arg = s, splitters

            match memo.TryFind arg with
            | Some res -> res
            | None ->
                let res =
                    match splitters with
                    | [] -> 1L
                    | head :: rest when Set.contains s head ->
                        let rCount = countPaths (s - 1) rest
                        let lCount = countPaths (s + 1) rest
                        lCount + rCount
                    | _ :: rest -> countPaths s rest in

                memo.Add(arg, res)
                res

        countPaths source splitters


    let solve () =
        printfn $"""%A{solveImpl1 "../Days/data/day07_example.txt"} ref(21)"""
        printfn $"""%A{solveImpl1 "../Days/data/day07.txt"} ref(1581)"""
        printfn $"""%A{solveImpl2 "../Days/data/day07_example.txt"} ref(40)"""
        printfn $"""%A{solveImpl2' "../Days/data/day07_example.txt"} ref(40)"""
        printfn $"""%A{solveImpl2 "../Days/data/day07.txt"} ref(73007003089792L)"""
        printfn $"""%A{solveImpl2' "../Days/data/day07.txt"} ref(73007003089792L)"""

module Day8 =
    type Coord = { x: int64; y: int64; z: int64 }

    let square x = x * x

    let distSquared c1 c2 =
        square (c1.x - c2.x) + square (c1.y - c2.y) + square (c1.z - c2.z)

    let parseLine line =
        line
        |> String.splitChar [| ',' |]
        |> Array.map Util.parseInt64
        |> function
            | [| Some x; Some y; Some z |] -> { x = x; y = y; z = z }
            | _ -> failwithf "failed to parse line '%s'" line


    let parseFile = File.ReadLines >> Seq.map parseLine >> Seq.toArray

    let sortedPairsWithDist coords =
        [| 0 .. (Array.length coords - 1) |]
        |> Array.collect (fun i -> [| 0 .. (i - 1) |] |> Array.map (fun j -> (j, i)))
        |> Array.map (fun (i, j) -> distSquared coords[i] coords[j], (i, j))
        |> Array.sort
        |> Array.map (fun (_, (i1, i2)) -> i1, i2)

    type ConnectionHelper2 =
        { iToSetId: Map<int, int>
          idToSet: Map<int, list<int>> }

    let connectIndices
        { iToSetId = iToSetId
          idToSet = idToSet }
        (i1, i2)
        =
        match Map.tryFind i1 iToSetId, Map.tryFind i2 iToSetId with
        | Some setId1, Some setId2 ->
            match setId1 = setId2 with
            | true ->
                { iToSetId = iToSetId
                  idToSet = idToSet }
            | false ->
                let set2 = idToSet[setId2]

                { iToSetId = set2 |> List.fold (fun map i -> Map.add i setId1 map) iToSetId
                  idToSet =
                    idToSet
                    |> Map.remove setId2
                    |> Map.change setId1 (Option.map (fun set1 -> set2 @ set1)) }
        | _, _ -> failwith "impossible"


    let solveImpl1 path n =
        let coords = parseFile path
        let inds = [| 0 .. Array.length coords - 1 |]
        let closest = sortedPairsWithDist coords |> Array.take n

        Array.fold
            connectIndices
            { iToSetId = inds |> Array.map (fun i -> i, i) |> Map
              idToSet = inds |> Array.map (fun i -> i, i :: []) |> Map }
            closest
        |> fun helper -> Map.values helper.idToSet
        |> Seq.map List.length
        |> Seq.toArray
        |> Array.sortDescending
        |> Array.take 3
        |> Array.reduce (*)

    let solveImpl2 path =
        let coords = parseFile path
        let inds = [| 0 .. Array.length coords - 1 |]
        let closest = sortedPairsWithDist coords

        let i =
            Seq.scan
                connectIndices
                { iToSetId = inds |> Array.map (fun i -> i, i) |> Map
                  idToSet = inds |> Array.map (fun i -> i, i :: []) |> Map }
                closest
            |> Seq.map (fun helper -> Map.values helper.idToSet)
            |> Seq.findIndex (fun sets -> Seq.length sets = 1 && Seq.head sets |> List.length |> (=) coords.Length)

        let i1, i2 = closest[i - 1] // the last pair we needed to add before being complete
        coords[i1].x * coords[i2].x



    let solve () =
        printfn $"""%A{solveImpl1 "../Days/data/day08_example.txt" 10} ref(40)"""
        printfn $"""%A{solveImpl1 "../Days/data/day08.txt" 1000} ref(57564)"""
        printfn $"""%A{solveImpl2 "../Days/data/day08_example.txt"} ref(25272)"""
        printfn $"""%A{solveImpl2 "../Days/data/day08.txt"} ref(133296744L)"""

module Day9 =
    let draw coords =
        let minX = coords |> List.minBy fst |> fst
        let maxX = coords |> List.maxBy fst |> fst
        let minY = coords |> List.minBy snd |> snd
        let maxY = coords |> List.maxBy snd |> snd

        [ minY..maxY ]
        |> List.map (
            (fun y ->
                [ minX..maxX ]
                |> List.map (fun x ->
                    List.sumBy ((=) (x, y) >> Util.boolToInt) coords
                    |> fun m -> if m > 0 then char m + '0' else '.'))
            >> List.toArray
        )
        |> List.map System.String
        |> String.concat "\n"
        |> fun str -> String.concat "\n" [ ""; str; "" ]


    let parseLine line =
        line
        |> String.splitChar [| ',' |]
        |> Array.map Util.parseInt64
        |> function
            | [| Some x; Some y |] -> x, y
            | _ -> failwithf "failed to parse line '%s'" line


    let parseFile = File.ReadLines >> Seq.map parseLine >> Seq.toList

    let area ((x1, y1), (x2, y2)) =
        (abs (x1 - x2) + 1L) * (abs (y1 - y2) + 1L)

    let areasAndEdgePairs edges =
        seq { 0 .. (List.length edges - 1) }
        |> Seq.collect (fun i -> seq { i + 1 .. (List.length edges - 1) } |> Seq.map (fun j -> i, j))
        |> Seq.map (fun (i, j) -> edges[i], edges[j])
        |> Seq.map (fun es -> area es, es)

    let solveImpl1 path =
        path |> parseFile |> areasAndEdgePairs |> Seq.maxBy fst |> snd |> area

    let solveImpl2 path =
        let redTiles = parseFile path
        let redTilesWrapAround = List.last redTiles :: redTiles

        let greenBorder =
            redTilesWrapAround
            |> List.pairwise
            |> List.collect (function
                | (x1, y1), (x2, y2) when y1 = y2 && x1 < x2 -> [ x1..x2 ] |> List.map (fun x -> (x, y1), 'l')
                | (x1, y1), (x2, y2) when y1 = y2 && x1 > x2 -> [ x2..x1 ] |> List.map (fun x -> (x, y1), 'r')
                | (x1, y1), (x2, y2) when x1 = x2 && y1 < y2 -> [ y1..y2 ] |> List.map (fun y -> (x1, y), 'u')
                | (x1, y1), (x2, y2) when x1 = x2 && y1 > y2 -> [ y2..y1 ] |> List.map (fun y -> (x1, y), 'd')
                | _ -> [])

        let crossings pickCoord pickOtherCoord ofChar border =
            border
            |> List.groupBy (fun (coord, _) -> pickCoord coord)
            |> List.map (fun (x, sameXs) ->
                x,
                sameXs
                |> List.filter (fun (_, dir) -> ofChar |> List.contains dir)
                |> List.map (fun (coord, d) -> pickOtherCoord coord, d)
                |> List.sortByDescending fst
                |> List.fold
                    (fun (prevDir, l) (y, dir) ->
                        match l with
                        | l when dir <> prevDir -> dir, y :: l
                        | h :: r when dir = prevDir -> dir, y :: r
                        | _ -> failwith "impossible")
                    (' ', [])
                |> snd
                |> List.pairwise // pair up coordinates where we change from inside to outside and vice versa
                |> Seq.mapi tuple2
                |> Seq.filter (fun (i, _) -> i % 1 = 0) // only keep even cases - those describing the intervals where we're inside
                |> Seq.map snd
                |> Seq.toList)
            |> Map

        let leftRight = [ 'l'; 'r' ]
        let upDown = [ 'u'; 'd' ]

        let crossingsInYOfX = crossings fst snd leftRight greenBorder

        let crossingsInXOfY = crossings snd fst upDown greenBorder

        let isValid ((x1, y1), (x2, y2)) =
            let xLimits = min x1 x2, max x1 x2
            let yLimits = min y1 y2, max y1 y2

            let isInbounds input crossingsMap limits =
                input
                |> List.map (fun x ->
                    crossingsMap
                    |> Map.tryFind x
                    |> Option.map (List.exists (fun (iStart, iEnd) -> iStart <= fst limits && snd limits <= iEnd)))
                |> List.forall (Option.getOrElse false)

            isInbounds [ fst xLimits; snd xLimits ] crossingsInYOfX yLimits
            && isInbounds [ fst yLimits; snd yLimits ] crossingsInXOfY xLimits

        areasAndEdgePairs redTiles
        |> Seq.toArray
        |> Array.sortDescending
        |> Array.tryFind (snd >> isValid)
        |> Option.map fst
        |> Option.getOrFail "oh no"

    let solve () =
        printfn $"""%A{solveImpl1 "../Days/data/day09_example.txt"} ref(50)"""
        printfn $"""%A{solveImpl1 "../Days/data/day09.txt"} ref(4748985168)"""
        printfn $"""%A{solveImpl2 "../Days/data/day09_example.txt"} ref(24)"""
        printfn $"""%A{solveImpl2 "../Days/data/day09.txt"} ref(1550760868L)"""

module Day10 =
    type IndicatorLights = array<bool>
    type Button = list<int>
    type Buttons = list<Button>
    type Joltages = array<int>

    let parseLine line =
        let parts = line |> String.splitChar [| ' ' |] |> Array.toList

        let indicatorLights: IndicatorLights =
            parts[0]
            |> String.toCharArray
            |> Array.choose (function
                | '#' -> Some true
                | '.' -> Some false
                | _ -> None)

        let buttons: Buttons =
            parts
            |> List.tail
            |> List.choose (function
                | str when String.startsWith "(" str ->
                    str
                    |> String.splitChar [| ','; '('; ')' |]
                    |> Array.toList
                    |> List.choose Util.parseInt
                    |> Some
                | _ -> None)

        let joltage: Joltages =
            parts
            |> List.last
            |> (function
            | str when String.startsWith "{" str ->
                str
                |> String.splitChar [| ','; '{'; '}' |]
                |> Array.choose Util.parseInt
                |> Some
            | _ -> None)
            |> Option.getOrFail "could not process joltage"

        indicatorLights, buttons, joltage

    let parseFile = File.ReadLines >> Seq.map parseLine >> Seq.toList

    let applyButton (indicatorLights: IndicatorLights) (button: Button) =
        indicatorLights
        |> Array.mapi (fun i v -> if button |> List.contains i then not v else v)

    let rec minimalPresses indicatorLights (buttons: Buttons) =
        // getting from initially all-false to the requested configuration requires the
        // same presses as vice versa and sequence of presses is irrelevant
        // pressing buttons twice undoes its action, so we may only consider pressing a button or not
        let allOff = Array.forall not indicatorLights

        match buttons, allOff with
        | _, true -> Some 0
        | [], _ -> None
        | head :: tail, _ ->
            match minimalPresses indicatorLights tail, minimalPresses (applyButton indicatorLights head) tail with
            | Some buttoneSkipped, Some buttonApplied -> Some(min buttoneSkipped (1 + buttonApplied))
            | None, Some buttonApplied -> Some(1 + buttonApplied)
            | Some buttoneSkipped, None -> Some buttoneSkipped
            | None, None -> None

    let solveImpl1 path =
        path
        |> parseFile
        |> List.map (fun (indicatorLights, buttons, _) -> minimalPresses indicatorLights buttons)
        |> List.choose id
        |> List.sum

    let applyButton' (joltages: Joltages) (button: Button) =
        joltages
        |> Array.mapi (fun i v -> if button |> List.contains i then v + 1 else v)

    let minimalPresses' (joltage, buttons) =
        //printfn "joltage: %A\n" joltage
        //buttons
        //|> List.map (fun button ->
        //[ 0 .. (Array.length joltage) ]
        //|> List.map (fun i -> List.contains i button |> Util.boolToInt))
        //|> printfn "buttons: %A\n"

        let allZeros = joltage |> Array.map (fun _ -> 0)

        // addition with carry but 'digits' are limited by joltage
        let increase ind =
            ind
            |> Array.zip joltage
            |> Array.rev
            |> Array.fold
                (fun (l, c) (x, y) ->
                    match x >= y + c with
                    | true -> y + c :: l, 0
                    | false -> 0 :: l, 1)
                ([], 1)
            |> fst
            |> List.toArray

        let increase' (ind: array<int>) table =
            let nextInd = table |> Map.keys |> Seq.find ((<) ind)
            nextInd

        let rec helper table i =
            // all I want is the next larger key
            let ind = Map.keys table |> Seq.skip i |> Seq.head

            match Map.tryFind ind table with
            | Some nSteps when ind = joltage -> nSteps
            | Some nSteps ->
                buttons
                |> List.map (applyButton' ind)
                |> List.filter (fun newJoltage -> Array.zip joltage newJoltage |> Array.forall (fun (x, y) -> x >= y))
                |> List.fold
                    (fun map newJoltage ->
                        map
                        |> Map.change newJoltage (function
                            | Some nStepsPrev -> Some(min (nSteps + 1) nStepsPrev)
                            | None -> Some(nSteps + 1)))
                    table
                |> fun newTable -> helper newTable (i + 1)
            | None -> failwith "oh no"

        let table = Map.empty |> Map.add allZeros 0
        helper table 0

    let solveImpl2 path =

        let lines = path |> parseFile

        lines
        |> List.map (fun (_, buttons, joltage) -> joltage, buttons)
        |> List.map (
            minimalPresses'
            >> fun x ->
                printfn "%A" x
                x
        )
        |> List.sum

    // * presses (still) commute
    // * once a joltage is 0, all buttons that contain it must be removed

    let solve () =
        printfn $"""%A{solveImpl1 "../Days/data/day10_example.txt"} ref(7)"""
        printfn $"""%A{solveImpl1 "../Days/data/day10.txt"} ref(550)"""
        printfn $"""%A{solveImpl2 "../Days/data/day10_example.txt"} ref(33)"""
        printfn $"""%A{solveImpl2 "../Days/data/day10.txt"} ref(1550760868L)"""

module Day11 =
    let parseLine line =
        let origin, targets =
            match line |> String.splitChar [| ':' |] with
            | [| origin; targets |] -> (String.trim origin, targets)
            | _ -> failwithf "unexpected input %A" line

        origin,
        targets
        |> String.splitChar [| ' ' |]
        |> Array.filter (String.IsNullOrEmpty >> not)

    let parseFile = File.ReadLines >> Seq.map parseLine >> Map

    let solveImpl1 path =
        let connections = parseFile path

        let rec helper from =
            match from with
            | "out" -> 1
            | _ ->
                match connections |> Map.tryFind from with
                | Some targets -> targets |> Array.sumBy (fun target -> helper target)
                | None -> 0

        helper "you"

    let solveImpl1' path =
        let connections = parseFile path

        let rec helper from visited =
            let newVisited = Set.add from visited

            match from with
            | _ when Set.contains from visited -> 0
            | "out" -> 1
            | _ ->
                match connections |> Map.tryFind from with
                | Some targets ->
                    targets
                    |> Array.filter ((fun node -> Set.contains node visited) >> not)
                    |> Array.sumBy (fun target -> helper target newVisited)
                | None -> 0

        helper "you" Set.empty

    let solveImpl2 path =
        let connections = parseFile path

        let memo = new Dictionary<_, _>()

        let rec helper from (visitedDac, visitedFFT) =
            match memo.TryFind(from, (visitedDac, visitedFFT)) with
            | Some res -> res
            | None ->
                let res =
                    match from with
                    | "out" -> if visitedDac && visitedFFT then 1L else 0L
                    | _ ->
                        match connections |> Map.tryFind from with
                        | Some targets ->
                            let updatedVisited = visitedDac || from = "dac", visitedFFT || from = "fft"
                            targets |> Array.sumBy (fun target -> helper target updatedVisited)
                        | None -> 0L

                memo.Add((from, (visitedDac, visitedFFT)), res)
                res

        helper "svr" (false, false)

    let solveImpl2' path =
        let connections = parseFile path

        let memo = new Dictionary<_, _>()

        let rec helper from visited =
            let visitedDac = Set.contains "dac" visited
            let visitedFFT = Set.contains "fft" visited

            match memo.TryFind(from, (visitedDac, visitedFFT)) with
            | Some res -> res
            | None ->
                let res =
                    match from with
                    | _ when Set.contains from visited -> 0L
                    | "out" -> (visitedDac && visitedFFT) |> Util.boolToInt |> int64
                    | _ ->
                        match connections |> Map.tryFind from with
                        | Some targets -> targets |> Array.sumBy (fun target -> helper target (Set.add from visited))
                        | None -> 0L

                memo.Add((from, (visitedDac, visitedFFT)), res)
                res

        helper "svr" Set.empty

    let solve () =
        Util.printTimedResult (fun () -> solveImpl1 "../Days/data/day11_example.txt") 5
        Util.printTimedResult (fun () -> solveImpl1' "../Days/data/day11_example.txt") 5

        Util.printTimedResult (fun () -> solveImpl1 "../Days/data/day11.txt") 431
        Util.printTimedResult (fun () -> solveImpl1' "../Days/data/day11.txt") 431

        Util.printTimedResult (fun () -> solveImpl2 "../Days/data/day11_example2.txt") 2
        Util.printTimedResult (fun () -> solveImpl2' "../Days/data/day11_example2.txt") 2

        Util.printTimedResult (fun () -> solveImpl2 "../Days/data/day11.txt") 358458157650450L
        Util.printTimedResult (fun () -> solveImpl2' "../Days/data/day11.txt") 358458157650450L
