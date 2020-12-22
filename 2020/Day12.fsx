#load "utils.fsx"

open Utils
open System.IO

let instructions =
    File.ReadAllLines(@"2020/day12_data.txt")
    |> Seq.toList

let turnRight current turnDegrees = (current + turnDegrees) % 360

let turnLeft current turnDegrees =
    let x = (current - turnDegrees) % 360
    if x >= 0 then x else 360 + x

let changeDirection current turnDirection turnDegrees =
    if turnDirection = "L"
    then turnLeft current turnDegrees
    else turnRight current turnDegrees

let moveNorth (east, north, direction) value = (east, north + value, direction)

let moveSouth (east, north, direction) value = (east, north - value, direction)

let moveEast (east, north, direction) value = (east + value, north, direction)

let moveWest (east, north, direction) value = (east - value, north, direction)

let moveForward (east, north, direction) value =
    match direction with
    | 0 -> moveNorth (east, north, direction) value
    | 90 -> moveEast (east, north, direction) value
    | 180 -> moveSouth (east, north, direction) value
    | 270 -> moveWest (east, north, direction) value
    | _ ->
        printfn "Unexpected direction %i" direction
        (east, north, direction)

let processInstruction (east, north, direction) instruction =
    let (command, v) = StringUtils.splitStringAt 1 instruction
    let value = int v

    match command with
    | "N" -> moveNorth (east, north, direction) value
    | "S" -> moveSouth (east, north, direction) value
    | "E" -> moveEast (east, north, direction) value
    | "W" -> moveWest (east, north, direction) value
    | "L"
    | "R" -> (east, north, (changeDirection direction command value))
    | "F" -> moveForward (east, north, direction) value

let manhattanDistance (east, north, _) = abs (east) + abs (north)

let partOneOutput =
    instructions
    |> List.fold processInstruction (0, 0, 90)
    |> manhattanDistance
