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
