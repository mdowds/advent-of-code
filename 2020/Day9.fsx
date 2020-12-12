open System.IO

let numbers =
    File.ReadAllLines(@"2020/day9_data.txt")
    |> Seq.toList
    |> List.map int64

let sumsTo target list n =
    List.exists (fun x -> x + n = target) list

let pairInListSumsTo list target = List.exists (sumsTo target list) list

let splitLast (list: 'a list) =
    let (rest, last) = List.splitAt (list.Length - 1) list
    (rest, List.head last)

let pairInRestSumsToLast list =
    let (candidates, target) = splitLast list
    pairInListSumsTo candidates target

let isFalse x = x = false

let partOneOutput =
    numbers
    |> List.windowed 26
    |> List.find (pairInRestSumsToLast >> isFalse)
    |> (fun list -> list.[25])

let foldAndTakeWhile predicate state list =
    let rec loop out state list =
        match list with
        | x :: xs ->
            match predicate state x with
            | (newState, true) -> loop (out @ [ x ]) newState xs
            | (newState, false) -> (newState, out)
        | _ -> (state, [])

    loop [] state list

let takeFrom i = List.splitAt i >> snd

let lessThanEqualTo target acc x =
    let newAcc = acc + x
    if newAcc <= target then (newAcc, true) else (acc, false)

let rec tryFindSeriesThatSumsTo (target: int64) numbers i =
    let (total, series) =
        numbers
        |> takeFrom i
        |> foldAndTakeWhile (lessThanEqualTo target) 0L

    if total = target
    then series
    else tryFindSeriesThatSumsTo target numbers (i + 1)

let minAndMax list = (List.min list, List.max list)

let partTwoOutput =
    tryFindSeriesThatSumsTo partOneOutput numbers 0
    |> minAndMax
    |> (fun (x, y) -> x + y)
