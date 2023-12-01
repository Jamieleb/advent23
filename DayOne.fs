module DayOne

open System.Text.RegularExpressions

let private digitPattern =
    "\d|(o)(?=(ne))|(tw)(?=(o))|(th)(?=(ree))|(fo)(?=(ur))|(fi)(?=(ve))|(si)(?=(x))|(se)(?=(ven))|(e)(?=(ight))|(n)(?=(ine))"

let private rx = Regex digitPattern

let private digitStrToInt =
    function
    | "o" -> 1
    | "tw" -> 2
    | "th" -> 3
    | "fo" -> 4
    | "fi" -> 5
    | "si" -> 6
    | "se" -> 7
    | "e" -> 8
    | "n" -> 9
    | _ as other -> int other

let private getDigits str =
    str
    |> rx.Matches
    |> Seq.cast
    |> Seq.map (fun (m: Match) -> m.Value)
    |> Seq.map digitStrToInt
    |> Seq.toList

let private firstAndLast list =
    List.map string [ List.head list; List.last list ]

let solve input =
    input
    |> Array.toSeq
    |> Seq.map getDigits
    |> Seq.map firstAndLast
    |> Seq.map (List.fold (+) "")
    |> Seq.map int
    |> Seq.sum
