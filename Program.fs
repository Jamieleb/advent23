﻿module Program

open System
open System.IO

let solve puzzle =
    let input = File.ReadAllLines $"./input/{puzzle}.txt"

    match puzzle with
    | "dayOne" -> printfn "%A" (DayOne.solve input)
    | "dayTwo" -> printfn "%A" (DayTwo.solve2 input)
    | "dayThree" -> printfn "%A" (DayThree.solve2 input)
    | "dayFour" -> printfn "%A" (DayFour.solve input)
    | _ -> printfn "%A" "Puzzle not found"

    ()

[<EntryPoint>]
let main args =
    let puzzleOpt = Array.tryHead args
    puzzleOpt |> Option.map solve |> ignore
    0
