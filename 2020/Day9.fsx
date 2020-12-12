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

let trySumTo target total el =
    match total with
    | Some total when total < target -> Some(total + el)
    | Some total when total = target -> Some total
    | _ -> None

let tryFindSeriesThatSumsTo target (numbers: int list) res i =
    let tryFindFromIndex i =
        numbers
        |> List.splitAt i
        |> snd
        |> fun xs -> List.fold (trySumTo target) (xs |> List.head |> Some) xs

    match res with
    | Some x -> x
    | None -> tryFindFromIndex i
