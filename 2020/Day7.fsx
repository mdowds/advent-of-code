open System
open System.IO

let rulesRaw =
    File.ReadAllLines(@"2020/day7_data.txt")
    |> Seq.toList

type Rule =
    { Container: string
      AllowedContents: (int * string) list }

let splitS (seps: string []) (s: string) = s.Split(seps, StringSplitOptions.None)
let splitC (seps: char []) (s: string) = s.Split(seps, StringSplitOptions.None)

let join (delimiter: string) (s: string []) = String.Join(delimiter, s)

let sub i len xs = Array.sub xs i len

let parseContainer =
    splitC [| ' ' |] >> Array.take 2 >> join " "

let parseContentsRule rule =
    let splitRule = rule |> splitC [| ' ' |]
    let quantity = splitRule |> Array.head |> int
    let bagType = splitRule |> sub 1 2 |> join " "
    (quantity, bagType)

let parseAllowedContents rule =
    match rule with
    | "no other bags." -> []
    | _ ->
        rule
        |> splitS [| ", "; "." |]
        |> Array.filter (fun x -> x <> "")
        |> Array.map parseContentsRule
        |> Array.toList

let parseRule (rule: string) =
    let components = splitS [| " contain " |] rule
    let container = components.[0] |> parseContainer
    let allowedContents = components.[1] |> parseAllowedContents

    { Container = container
      AllowedContents = allowedContents }

let rec findContainers bagType rules matches =
    let findContainersWithNewMatches (r: Rule): string list =
        List.append [ r.Container ] matches
        |> findContainers r.Container rules

    let ruleInvolvesBagType bagType rule =
        let ruleMatches =
            List.filter (fun (_, bt) -> bt = bagType) rule.AllowedContents

        Seq.length ruleMatches > 0

    match List.filter (ruleInvolvesBagType bagType) rules with
    | [] -> matches
    | filteredRules ->
        filteredRules
        |> List.collect findContainersWithNewMatches
        |> Seq.distinct
        |> Seq.toList

let rules = rulesRaw |> List.map parseRule

let partOneOutput =
    findContainers "shiny gold" rules []
    |> List.length

let rec findContentsQuantity bagType rules =
    let rule =
        List.find (fun r -> r.Container = bagType) rules

    match rule.AllowedContents with
    | [] -> 0
    | ac ->
        let totalBags (q, bt) = q + (q * findContentsQuantity bt rules )
        List.map totalBags ac |> List.sum
        
let partTwoOutput =
    findContentsQuantity "shiny gold" rules
