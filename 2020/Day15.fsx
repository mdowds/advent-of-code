let data = [ 5; 1; 9; 18; 13; 8; 0 ]

let rec takeRound turn lastNum (turnsSpoken: Map<int, int list>) =
    let updateTurnsSpoken (turnsSpoken: Option<int list>) =
        match turnsSpoken with
        | Some x -> Some(turn :: x)
        | None -> Some [ turn ]

    let newNum =
        match Map.tryFind lastNum turnsSpoken with
        | Some previouslySpokenOn when List.length previouslySpokenOn > 1 ->
            previouslySpokenOn.[0] - previouslySpokenOn.[1]
        | _ -> 0


    let newTurnsSpoken =
        turnsSpoken |> Map.change newNum updateTurnsSpoken

    if turn = 2020
    then newNum
    else takeRound (turn + 1) newNum newTurnsSpoken

let initialTurns =
    data
    |> List.mapi (fun i x -> (x, [ i + 1 ]))
    |> Map

let partOneOutput =
    takeRound (List.length data + 1) (List.last data) initialTurns
