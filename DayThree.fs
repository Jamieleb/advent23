module DayThree

open System.Text.RegularExpressions

let sampleInput =
    [| "467..114.."
       "...*......"
       "..35..633."
       "......#..."
       "617*......"
       ".....+.58."
       "..592....."
       "......755."
       "...$.*...."
       ".664.598.." |]

let symbolPattern = "[^\d\.]+"

let symbolRegex = Regex symbolPattern

let gearPattern = "\*"

let gearRegex = Regex gearPattern

let numberPattern = "\d+"

let numberRegex = Regex numberPattern

type Position =
    { X: int
      Y: int
      Length: int
      Value: string }

let setFromPosition shouldExtend position =
    let offset = if shouldExtend then 1 else 0
    Set.ofList [ position.X - offset .. position.X + position.Length + offset - 1 ]

let isNumAdjToSymbol numPosition symPosition =
    let numSet = setFromPosition false numPosition

    setFromPosition true symPosition
    |> Set.intersect (setFromPosition false numPosition)
    |> Set.isEmpty
    |> not

let isAdjRow target position =
    let { Y = posRow } = position
    posRow >= target - 1 && posRow <= target + 1

let positionsInAdjRows targetPosition positionSeq =
    let { Y = targetRow } = targetPosition

    positionSeq |> Seq.filter (isAdjRow targetRow)

let isNumAdjToAnySymbols symPositions numPosition =
    symPositions
    |> positionsInAdjRows numPosition
    |> Seq.exists (isNumAdjToSymbol numPosition)

let numbersAdjToGear numPositions gearPosition =
    numPositions
    |> positionsInAdjRows gearPosition
    |> Seq.filter (fun numPs -> isNumAdjToSymbol numPs gearPosition)

let rowToPosition (rx: Regex) (y, row) =
    row
    |> rx.Matches
    |> Seq.cast
    |> Seq.map (fun (m: Match) ->
        { X = m.Index
          Y = y
          Length = m.Length
          Value = m.Value })

let positionsByRegex rx input =
    input |> Seq.map (rowToPosition rx) |> Seq.concat

let inputToIndexedSeq input = input |> Seq.ofArray |> Seq.indexed

let solve input =
    let indexedInput = inputToIndexedSeq input

    let symPositions = positionsByRegex symbolRegex indexedInput

    indexedInput
    |> positionsByRegex numberRegex
    |> Seq.filter (isNumAdjToAnySymbols symPositions)
    |> Seq.map (fun p -> int p.Value)
    |> Seq.sum

let gearRatio numPositionSeq =
    match List.ofSeq numPositionSeq with
    | [ numP1; numP2 ] -> int numP1.Value * int numP2.Value
    | _ -> 0

let solve2 input =
    let indexedInput = inputToIndexedSeq input

    let numPositions = positionsByRegex numberRegex indexedInput

    indexedInput
    |> positionsByRegex gearRegex
    |> Seq.map (numbersAdjToGear numPositions)
    |> Seq.filter (fun nums -> Seq.length nums = 2)
    |> Seq.map gearRatio
    |> Seq.sum
