open System
open System.IO

let lines =
    File.ReadAllLines(@"2020/day8_data.txt")
    |> Seq.toList

type Outcome =
    | Loop of int
    | Exit of int

let splitC (seps: char []) (s: string) = s.Split(seps, StringSplitOptions.None)

let parseLine i line =
    let components = line |> splitC [| ' ' |]
    (i, (components.[0], (int components.[1])))

let rec processInstruction instructions acc linesRun lineNo =
    let executeInstruction lineNo =
        let (instruction, value) = Map.find lineNo instructions
        match instruction with
        | "acc" -> processInstruction instructions (acc + value) (lineNo :: linesRun) (lineNo + 1)
        | "jmp" -> processInstruction instructions acc (lineNo :: linesRun) (lineNo + value)
        | _ -> processInstruction instructions acc (lineNo :: linesRun) (lineNo + 1)

    match lineNo with
    | lineNo when List.contains lineNo linesRun -> Loop acc
    | lineNo when lineNo = Map.count instructions -> Exit acc
    | _ -> executeInstruction lineNo

let processInstructions instructions = processInstruction instructions 0 [] 0

let instructions = List.mapi parseLine lines |> Map

let partOneOutput = processInstructions instructions

let swapInstruction allInstructions lineNo (instruction, value) =
    match instruction with
    | "nop" -> Some(Map.add lineNo ("jmp", value) allInstructions)
    | "jmp" -> Some(Map.add lineNo ("nop", value) allInstructions)
    | _ -> None

let instructionsToTry =
    instructions
    |> Map.map (swapInstruction instructions)
    |> Map.filter (fun _ v -> v.IsSome)
    |> Map.map (fun _ v -> v.Value)
    |> Map.toList

let producesExit (_, instructions) =
    match processInstructions instructions with
    | Loop _ -> None
    | Exit acc -> Some acc

let partTwoOutput =
    List.tryPick producesExit instructionsToTry
