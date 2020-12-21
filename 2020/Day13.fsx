open System.IO

let data =
    File.ReadAllLines(@"2020/day13_data.txt")

let startTime = data.[0] |> int

let busIDs =
    data.[1]
    |> (fun s -> s.Split(','))
    |> Array.filter (fun id -> id <> "x")
    |> Array.map int

let rec nextDeparture currentDeparture interval =
    let next = currentDeparture + interval
    if next > startTime then (interval, next) else nextDeparture next interval

let partOneOutput =
    busIDs
    |> Array.map (nextDeparture 0)
    |> Array.minBy snd
    |> (fun (id, departure) -> id * (departure - startTime))
