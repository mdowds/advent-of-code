open System.IO

let lines =
    File.ReadAllLines(@"2020/day3_data.txt") |> Seq.toList

let toCharArray (s: string) = s.ToCharArray()
let countIf f = Seq.filter f >> Seq.length
let isTrue x = x = true

let steps = Seq.map toCharArray lines

let getAdjustedXPos stepsRight stepLength yPos =
    let xPos = yPos * stepsRight

    match xPos >= stepLength with
    | true -> xPos % stepLength
    | false -> xPos

let hasTree stepsRight yPos (step: char []) =
    let xPos =
        getAdjustedXPos stepsRight (Seq.length step) yPos

    step.[xPos] = '#'

let countIfHasTree stepsRight =
    Seq.mapi (hasTree stepsRight) >> countIf isTrue

let stepWillBeTaken stepsDown stepIndex = stepIndex % stepsDown = 0

let getStepsToTake stepsDown steps =
    steps
    |> Seq.mapi (fun i x -> (i, x))
    |> Seq.filter (fun (i, _) -> stepWillBeTaken stepsDown i)
    |> Seq.map (fun (_, s) -> s)

let partOneOutput = steps |> countIfHasTree 3

let r1d1 = steps |> countIfHasTree 1
let r3d1 = partOneOutput
let r5d1 = steps |> countIfHasTree 5
let r7d1 = steps |> countIfHasTree 7

let r1d2 =
    steps |> getStepsToTake 2 |> countIfHasTree 1

let partTwoOutput =
    [ r1d1; r3d1; r5d1; r7d1; r1d2 ]
    |> Seq.map int64
    |> Seq.reduce (*)
