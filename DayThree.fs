module DayThree

open System.Text.RegularExpressions
open Utils.Tap

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


let isNumAdjToAnySymbols symPositions numPosition =
    let { Y = numRow } = numPosition

    symPositions
    |> Seq.filter (fun { Y = symRow } -> symRow >= numRow - 1 && symRow <= numRow + 1)
    |> Seq.exists (isNumAdjToSymbol numPosition)


let rowToPosition (rx: Regex) y row =
    row
    |> rx.Matches
    |> Seq.cast
    |> Seq.map (fun (m: Match) ->
        { X = m.Index
          Y = y
          Length = m.Length
          Value = m.Value })

let symbolPositions (rowOne: string, rowTwo: string) =
    [ 0, rowOne; 1, rowTwo ]
    |> List.map (fun (y, row) -> rowToPosition symbolRegex y row)
    |> Seq.concat

let numPositions (rowOne: string, rowTwo: string) =
    [ 0, rowOne; 1, rowTwo ]
    |> List.map (fun (y, row) -> rowToPosition numberRegex y row)
    |> Seq.concat

let getValidNumsFromRowPair rowPair =
    let symSet = symbolPositions rowPair

    rowPair
    |> numPositions
    |> Seq.filter (isNumAdjToAnySymbols symSet)
    |> Seq.map (fun p -> int p.Value)

let solve input =
    let indexedInput = Array.indexed input

    let symPositions =
        indexedInput
        |> Array.map (fun (i, row) -> rowToPosition symbolRegex i row)
        |> Seq.ofArray
        |> Seq.concat

    let numPositions =
        indexedInput
        |> Array.map (fun (i, row) -> rowToPosition numberRegex i row)
        |> Seq.ofArray
        |> Seq.concat

    numPositions
    |> Seq.filter (isNumAdjToAnySymbols symPositions)
    |> Seq.map (fun p -> int p.Value)
    |> Seq.sum
