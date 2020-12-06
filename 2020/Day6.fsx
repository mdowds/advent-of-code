open System.IO

let lines =
    File.ReadAllLines(@"2020/day6_data.txt")
    |> Seq.toList

let appendLine acc (line: string) =
    match acc with
    | [] -> [ [ line.ToCharArray() ] ]
    | h :: t -> (List.append [ line.ToCharArray() ] h) :: t

let combineLines acc line =
    match line with
    | "" -> [] :: acc
    | _ -> appendLine acc line

let responses = lines |> Seq.fold combineLines []

let partOneOutput =
    responses
    |> Seq.map (Array.concat >> Set >> Seq.length)
    |> Seq.sum
    
let commonArrayElements arr =
    let checkCandidates candidates currentSeq =
        Array.filter (fun elemToCheck -> Array.contains elemToCheck currentSeq) candidates
    Seq.reduce checkCandidates arr

let partTwoOutput =
    responses |> Seq.map (commonArrayElements >> Seq.length) |> Seq.sum
    