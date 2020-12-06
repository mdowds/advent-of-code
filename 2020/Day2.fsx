open System.IO
open System.Text.RegularExpressions

type PasswordPolicy =
    { RequiredChar: char
      FirstNumber: int
      SecondNumber: int }

let countIf f = Seq.filter f >> Seq.length

let lines =
    File.ReadAllLines(@"2020/day2_data.txt") |> Seq.toList

let getComponents line =
    let regex = (Regex "(\d+)-(\d+) (\w): (\w+)")
    let matches = regex.Match line
    let groups = matches.Groups

    let policy =
        { RequiredChar = groups.[3].Value.ToCharArray().[0]
          FirstNumber = groups.[1].Value |> int
          SecondNumber = groups.[2].Value |> int }

    (policy, groups.[4].Value)

let isValidForPartOne (policy: PasswordPolicy, password: string) =
    let occurrences =
        countIf (fun char -> char = policy.RequiredChar) (password.ToCharArray())

    occurrences >= policy.FirstNumber && occurrences <= policy.SecondNumber

let partOneOutput =
    lines |> Seq.map getComponents |> countIf isValidForPartOne

let isValidForPartTwo (policy: PasswordPolicy, password: string) =
    let chars = password.ToCharArray()
    let matchCount =  [policy.FirstNumber - 1; policy.SecondNumber - 1]
                      |> countIf (fun i -> chars.[i] = policy.RequiredChar)
    matchCount = 1

let partTwoOutput =
    lines |> Seq.map getComponents |> countIf isValidForPartTwo
