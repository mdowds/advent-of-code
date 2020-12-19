open System.IO

let toCharArray (str: string) = str.ToCharArray()

let rows =
    File.ReadAllLines(@"2020/day11_data.txt")
    |> Seq.toArray
    |> Array.map toCharArray


let countIf f = Array.filter f >> Array.length

let isSeatOccupied seat = seat = '#'

let windowed (all: char [] []) i el =
    match i with
    | 0 -> [| [||]; el; all.[i + 1] |]
    | i when i = (all.Length - 1) -> [| all.[i - 1]; el; [||] |]
    | _ -> [| all.[i - 1]; el; all.[i + 1] |]

let shouldBeVacated getSeatsToCheck limit col seat =
    match (col, seat) with
    | (_, 'L') -> false
    | (col, _) ->
        col
        |> getSeatsToCheck
        |> countIf isSeatOccupied
        |> (fun occupiedSeats -> occupiedSeats >= limit)

let canBeOccupied getSeatsToCheck col seat =
    let allSeatsUnoccupied col =
        col
        |> getSeatsToCheck
        |> Array.exists isSeatOccupied
        |> not

    match (col, seat) with
    | (_, '#') -> false
    | (col, _) -> allSeatsUnoccupied col
    
let processSpace getSeatsToCheck limit col space =
    match (col, space) with
    | (_, '.') -> '.'
    | (col, seat) when seat = 'L' -> if canBeOccupied getSeatsToCheck col seat then '#' else 'L'
    | (col, seat) when seat = '#' -> if shouldBeVacated getSeatsToCheck limit col seat then 'L' else '#'

let processWindow (window: char [] []) =
    let prev = window.[0]
    let row = window.[1]
    let next = window.[2]

    let adjacentSeats col =
        [| Array.tryItem (col - 1) prev
           Array.tryItem (col) prev
           Array.tryItem (col + 1) prev
           Array.tryItem (col - 1) row
           Array.tryItem (col + 1) row
           Array.tryItem (col - 1) next
           Array.tryItem (col) next
           Array.tryItem (col + 1) next |]
        |> Array.filter (fun x -> x.IsSome)
        |> Array.map (fun x -> x.Value)

    Array.mapi (processSpace adjacentSeats 4) row

let countOccupiedSeats (rows: char [] []) =
    rows |> Array.concat |> countIf (fun s -> s = '#')

let rec runPartOneSimulation prev =
    let current =
        prev
        |> Array.mapi (windowed prev)
        |> Array.map processWindow

    if current = prev then countOccupiedSeats current else runPartOneSimulation current

let partOneOutput = runPartOneSimulation rows

let processRow (allRows: char [] []) (rowIndex: int) (row: char []) =
    let tryGetSeat (row, col) =
        match Array.tryItem row allRows with
        | Some r -> Array.tryItem col r
        | None -> None

    let rec findVisibleSeat getCoordsOfNext (row, col) =
        let nextCoords = getCoordsOfNext (row, col)

        match tryGetSeat nextCoords with
        | Some '.' -> findVisibleSeat getCoordsOfNext nextCoords
        | seat -> seat

    let visibleSeats col =
        [| (rowIndex, col)
           |> findVisibleSeat (fun (r, c) -> (r - 1, c)) // Up
           (rowIndex, col)
           |> findVisibleSeat (fun (r, c) -> (r - 1, c + 1)) // Up right
           (rowIndex, col)
           |> findVisibleSeat (fun (r, c) -> (r, c + 1)) // Right
           (rowIndex, col)
           |> findVisibleSeat (fun (r, c) -> (r + 1, c + 1)) // Down right
           (rowIndex, col)
           |> findVisibleSeat (fun (r, c) -> (r + 1, c)) // Down
           (rowIndex, col)
           |> findVisibleSeat (fun (r, c) -> (r + 1, c - 1)) // Down left
           (rowIndex, col)
           |> findVisibleSeat (fun (r, c) -> (r, c - 1)) // Left
           (rowIndex, col)
           |> findVisibleSeat (fun (r, c) -> (r - 1, c - 1)) |] // Up left
        |> Array.filter (fun x -> x.IsSome)
        |> Array.map (fun x -> x.Value)

    row |> Array.mapi (processSpace visibleSeats 5)

let rec runPartTwoSimulation (prev: char [] []) =
    let current = prev |> Array.mapi (processRow prev)
    if current = prev then countOccupiedSeats current else runPartTwoSimulation current

let partTwoOutput = runPartTwoSimulation rows
