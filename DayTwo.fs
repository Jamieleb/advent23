module DayTwo

open System

type CubeCount =
    | Red of int
    | Green of int
    | Blue of int

let CubeCount count colour =
    match colour with
    | "red" -> Some(Red(int count))
    | "blue" -> Some(Blue(int count))
    | "green" -> Some(Green(int count))
    | _ -> None

// expected input [|" 3 blue"; " 2 red"|]
let getCubeCounts (strs: array<string>) =
    strs
    |> Array.map (fun s -> s.Trim())
    |> Array.map (fun s -> s.Split ' ')
    |> Array.map (function
        | [| count; colour |] -> CubeCount count colour
        | _ -> None)

type Reveal = { Red: int; Green: int; Blue: int }

let isRevealPossible maxReveals reveal =
    let { Red = maxRed
          Blue = maxBlue
          Green = maxGreen } =
        maxReveals

    let { Red = red
          Blue = blue
          Green = green } =
        reveal

    red <= maxRed && blue <= maxBlue && green <= maxGreen

let updateRevealWithCount reveal cubeCount =
    match cubeCount with
    | Red count -> { reveal with Red = count }
    | Blue count -> { reveal with Blue = count }
    | Green count -> { reveal with Green = count }

let revealFromCubeCountOpts opts =
    let folder accReveal current =
        match current with
        | None -> accReveal
        | Some cc -> updateRevealWithCount accReveal cc

    opts |> Array.fold folder { Red = 0; Blue = 0; Green = 0 }

let getReveals (str: string) =
    str
    |> fun s -> s.Split ';'
    |> Array.map (fun s -> s.Split ',')
    |> Array.map getCubeCounts
    |> Array.map revealFromCubeCountOpts

type Game = { Id: int; RevealList: array<Reveal> }

let foo = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

let bar = foo.Split ':'

let getId (str: string) =
    str |> String.filter Char.IsDigit |> int

let gameOpt (str: string) =
    let strArr = str.Split ':'
    let idOpt = strArr |> Array.tryHead |> Option.map getId
    let revealsOpt = strArr |> Array.tryLast |> Option.map getReveals

    match idOpt, revealsOpt with
    | Some id, Some reveals -> Some { Id = id; RevealList = reveals }
    | _ -> None

let isGamePossible maxReveals game =
    let { RevealList = reveals } = game

    reveals
    |> Array.map (isRevealPossible maxReveals)
    |> Array.fold (fun acc b -> acc && b) true

let maxReveals = { Red = 12; Green = 13; Blue = 14 }

let solve1 input =
    input
    |> Array.toSeq
    |> Seq.map gameOpt
    |> Seq.filter (fun gameOpt ->
        match gameOpt with
        | Some game -> isGamePossible maxReveals game
        | None -> false)
    |> Seq.map (Option.map (fun g -> g.Id))
    |> Seq.map (Option.defaultValue 0)
    |> Seq.sum

let minCubes (game: Game) =
    let revealList = game.RevealList |> Array.toList

    let head, tail =
        match revealList with
        | [] -> { Red = 0; Green = 0; Blue = 0 }, []
        | h :: t -> h, t

    let folder acc current =
        let { Red = accRed
              Blue = accBlue
              Green = accGreen } =
            acc

        let { Red = curRed
              Blue = curBlue
              Green = curGreen } =
            current

        { Red = max accRed curRed
          Blue = max accBlue curBlue
          Green = max accGreen curGreen }

    tail |> List.fold folder head

let powerOfReveal reveal = reveal.Red * reveal.Blue * reveal.Green

let gameMinCubesPower =
    gameOpt >> (Option.map minCubes) >> (Option.map powerOfReveal)

let solve2 input =
    input
    |> Array.toSeq
    |> Seq.map gameMinCubesPower
    |> Seq.map (Option.defaultValue 0)
    |> Seq.sum
