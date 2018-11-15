module App

open System
open System.Text.RegularExpressions
open Fable.Import
open Fable.PowerPack
module Node = Node.Exports

let ALL = @"(?:.|\n)"
let LINE = @"\r?\n"

let RECORD = sprintf @"type (\w+)%s*?{(%s*?)}" ALL ALL

let inline resolvePath relativePath =
    Node.path.join(Node.Globals.__dirname, relativePath)

let cleanComments (line: string) =
    let line =
        match line.IndexOf("//") with
        | -1 -> line
        | 0 -> ""
        | i -> line.[..(i-1)]
    if String.IsNullOrWhiteSpace(line)
    then None
    else line.Trim() |> Some

let findRecords txt =
    Regex.Matches(txt, RECORD)
    |> Seq.cast<Match>
    |> Seq.map (fun (m: Match) ->
        let name = m.Groups.[1].Value
        let body = m.Groups.[2].Value
        let keyValues =
            Regex.Split(body, LINE)
            |> Array.choose (fun line ->
                cleanComments line
                |> Option.bind (fun line ->
                    match line.Split(':') with
                    | [|part1; part2|] -> Some(part1.Trim(), part2.Trim())
                    | _ -> None
                ))
        name, Map keyValues
    )

let run() =
    let file =
        "${entryDir}../temp/Sample.fs"
        |> resolvePath
        |> Node.fs.readFileSync
        |> string
    for (name, members) in findRecords file do
        printfn "RECORD %s" name
        for KeyValue(k,v) in members do
            printfn "> %s: %s" k v

run()