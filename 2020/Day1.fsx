﻿open System.IO

// Builds out the list of all combinations before checking which sums to 2020
// So not the most efficient solution
let lines = File.ReadAllLines(@"day1_data.txt")
let nums = Seq.map int lines |> Seq.toList

// combinations taken from http://www.danielbradley.net/blog/2011/12-20-creating-possible-combinations/index.html
let rec combinations (l) =
    match l with
    | [] -> []
    | h::[] -> h |> List.map (fun opt -> [opt])
    | h::t ->
        combinations t
        |> List.map (fun tOpts ->
            h |> List.map (fun hOpt -> hOpt ::tOpts))
        |> List.concat
        
let output = combinations [nums; nums; nums]
             |> Seq.find (fun c -> Seq.sum c = 2020)
             |> Seq.reduce (*)
