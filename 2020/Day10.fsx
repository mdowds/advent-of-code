open System.IO

let adapters =
    File.ReadAllLines(@"2020/day10_data.txt")
    |> Seq.toList
    |> List.map int


let findNextAdapter all current =
    let rec findMatch targetDifference =
        match List.tryFind (fun x -> x - current = targetDifference) all with
        | Some adapter -> (adapter, targetDifference)
        | None -> findMatch (targetDifference + 1)

    if current = List.max all then (current + 3, 3) else findMatch 1

let partOneOutput =
    adapters
    |> (fun rs -> 0 :: rs)
    |> List.map (findNextAdapter adapters >> snd)
    |> List.countBy (fun x -> x)
    |> Map
    |> (fun map -> Map.find (1) map * Map.find (3) map)

// Attempted solution which takes too long to complete
let areAdaptersCompatible x y = y - x > 0 && y - x < 4

let getCompatibleAdapters all current =
    all |> List.filter (areAdaptersCompatible current)

let rec buildCombinations (combinationCount: int64) (current: int) (allCompatibles: Map<int, int list>) =
    match Map.tryFind current allCompatibles with
    | Some compatibleAdapters ->
        match compatibleAdapters with
        | [] ->
            combinationCount + 1L
        | _ ->
            compatibleAdapters
            |> List.fold (fun count ca -> buildCombinations count ca allCompatibles) combinationCount
    | None -> combinationCount + 1L

let partTwoOutput =
    adapters
    |> (fun rs -> 0 :: rs)
    |> List.map (fun a -> (a, getCompatibleAdapters adapters a))
    |> Map
    |> buildCombinations 0L 0
