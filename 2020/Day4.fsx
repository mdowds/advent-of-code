open System.IO
open System.Text.RegularExpressions

let lines =
    File.ReadAllLines(@"2020/day4_data.txt")
    |> Seq.toList


// PART 1

let countIf f = Seq.filter f >> Seq.length

let appendLine acc line =
    match acc with
    | [] -> [ line ]
    | h :: t ->
        match h with
        | "" -> line :: t
        | _ -> (h + " " + line) :: t


let combineLines acc line =
    match line with
    | "" -> "" :: acc
    | _ -> appendLine acc line

let fieldToPair (field: string) =
    let fieldComponents = field.Split ':'
    (fieldComponents.[0], fieldComponents.[1])

let passportStrToMap (str: string) =
    str.Split ' ' |> Seq.map fieldToPair |> Map

let requiredFields =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]

let isValid (passport: Map<string, string>) =
    Seq.forall (fun requiredField -> passport.ContainsKey requiredField) requiredFields
    
let passports =
    Seq.fold combineLines [] lines
    |> Seq.map passportStrToMap

let partOneOutput = countIf isValid passports


// PART TWO

let isByrValid (byr: string) = int byr >= 1920 && int byr <= 2002

let isIyrValid (iyr: string) = int iyr >= 2010 && int iyr <= 2020

let isEyrValid (eyr: string) = int eyr >= 2020 && int eyr <= 2030

let regexMatch regex =
    let regex = (Regex regex)
    regex.Match

let doesMatchRegex regex str =
    let rMatch = regexMatch regex str
    rMatch.Success

let isHgtValid hgt =
    let isMeasurementValid (groups: GroupCollection) =
        let measurement = int groups.[1].Value
        let unit = groups.[2].Value

        match unit with
        | "in" -> measurement >= 56 && measurement <= 76
        | "cm" -> measurement >= 150 && measurement <= 193
        | _ -> false

    let rMatch = regexMatch "(\d+)(in|cm)" hgt
    match rMatch.Success with
    | true -> isMeasurementValid rMatch.Groups
    | false -> false


let isHclValid = doesMatchRegex "#[0-9a-f]{6}"

let validEcls =
    [ "amb"
      "blu"
      "brn"
      "gry"
      "grn"
      "hzl"
      "oth" ]

let isEclValid ecl = Seq.contains ecl validEcls

let isPidValid = doesMatchRegex "^[0-9]{9}$"

let fieldValidators =
    Map [ ("byr", isByrValid)
          ("iyr", isIyrValid)
          ("eyr", isEyrValid)
          ("hgt", isHgtValid)
          ("hcl", isHclValid)
          ("ecl", isEclValid)
          ("pid", isPidValid)
          ("cid", (fun _ -> true)) ]

let isValidStrict passport =
    let validate k v = v |> fieldValidators.Item k
    Map.forall validate passport

let partTwoOutput =
    countIf (fun p -> isValid p && isValidStrict p) passports
