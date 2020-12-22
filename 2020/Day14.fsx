open System
open System.IO
open System.Text.RegularExpressions

let data =
    File.ReadAllLines(@"2020/day14_data.txt")

let splitString sep (str: string) = str.Split(sep)

let removeSubstring substr (str: string) = str.Replace(substr, "")

let toCharArray (s: string) = s.ToCharArray()

let regexMatch regex =
    let regex = (Regex regex)
    regex.Match

let firstCaptureGroup regex str =
    let matches = regexMatch regex str
    matches.Groups.[1].Value

let toInstruction (line: string []) = (line.[0], line.[1])

let parseLine line =
    line
    |> removeSubstring " "
    |> splitString [| '=' |]
    |> toInstruction

let extractAddress (command: string) =
    command |> firstCaptureGroup "mem\[(\d+)\]" |> int

let toBinary len (x: int64) = Convert.ToString(x, 2).PadLeft(len, '0')

let binaryToInt64 x = Convert.ToInt64(x, 2)

let applyMaskChar maskChar bitChar =
    match maskChar with
    | '1' -> '1'
    | '0' -> '0'
    | _ -> bitChar

let applyMask mask x =
    x
    |> int64
    |> toBinary 36
    |> toCharArray
    |> Array.map2 applyMaskChar mask
    |> String
    |> binaryToInt64

let processInstruction (mem: Map<int, int64>, mask: char []) (command, value) =
    let processMemInstruction address =
        Map.add address (applyMask mask value) mem

    match command with
    | "mask" -> (mem, toCharArray value)
    | memCommand when memCommand.StartsWith("mem") ->
        let newMem =
            processMemInstruction (extractAddress memCommand)

        (newMem, mask)
    | _ -> (mem, mask)

let initialMask =
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    |> toCharArray

let partOneOutput =
    data
    |> Array.map parseLine
    |> Array.fold processInstruction (Map.empty, initialMask)
    |> fst
    |> Map.fold (fun sum _ x -> sum + x) 0L
