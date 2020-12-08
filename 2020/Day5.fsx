open System.IO

let passes =
    File.ReadAllLines(@"2020/day5_data.txt")
    |> Seq.toList

let toCharArray (s: string) = s.ToCharArray()

let rows = { 0 .. 127 } |> Seq.toList
let cols = { 0 .. 7 } |> Seq.toList

let partitionList lowerIndicator l indicator =
    let halves = List.splitInto 2 l
    if indicator = lowerIndicator then halves.[0] else halves.[1]

let passCoord lowerIndicator l =
    toCharArray
    >> Seq.toList
    >> Seq.fold (partitionList lowerIndicator) l
    >> Seq.head

let seatId (pass: string) =
    let row =
        pass.Substring(0, 7) |> passCoord 'F' rows

    let col =
        pass.Substring(7, 3) |> passCoord 'L' cols

    row * 8 + col

let seatIds = passes |> Seq.map seatId

let partOneOutput = Seq.max seatIds

let partTwoOutput =
    Seq.sort seatIds
    |> Seq.pairwise
    |> Seq.find (fun (prev, curr) -> curr - prev > 1)
    |> (fun (prev, _) -> prev + 1)
