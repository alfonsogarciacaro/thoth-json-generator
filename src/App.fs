module App

open System
open System.Text.RegularExpressions
open Fable.Import
open Fable.PowerPack
module Node = Node.Exports

let SEPARATOR = """
==========================================================
"""

let ALL = @"(?:.|\n)"
let LINE = @"\r?\n"

let RECORD = Regex(@"type (\w+)\s*=\s*{((?:.|\n)*?)}")
let UNION = Regex(@"type (\w+)\s*=\s*\n((?:\s*(?:\/\/|\|).*\n)+)")
let UNION_CASE = Regex(@"^\s*\|\s*(\w*)(?: of (.+))?$")

let RECORD_ENCODER_TEMPLATE = """
    static member Encode(x: [NAME]) =
        Encode.object [[FIELDS]
        ]"""

let RECORD_FIELD_ENCODER_TEMPLATE = """
            "[NAME]", [TYPE] x.[NAME]"""

let RECORD_DECODER_TEMPLATE = """
    static member Decoder : Decode.Decoder<[NAME]> =
        Decode.object
            (fun get -> {[FIELDS]
            })"""

let RECORD_FIELD_DECODER_TEMPLATE = """
                [NAME] = get.Required.Field "[NAME]" [TYPE]"""

let UNION_ENCODER_TEMPLATE = """
    static member Encode(x: [NAME]) =
        match x with[CASES]"""

let UNION_FIELD_ENCODER_TEMPLATE = """
        | [NAME][FIELDS1] -> Encode.array [|Encode.string "[NAME]"[FIELDS2]|]"""

let UNION_DECODER_TEMPLATE = """
    static member Decode =
        Decode.index 0 Decode.string |> Decode.andThen (function[CASES]
            | _ -> Decode.fail "unkown")"""

let UNION_CASE_NO_FIELDS_DECODER_TEMPLATE = """
            | "[NAME]" -> Decode.succeed [NAME]"""

let UNION_CASE_DECODER_TEMPLATE = """
            | "[NAME]" ->  Decode.map[FIELD_COUNT] (fun [FIELD_CURRIED] -> [NAME]([FIELD_TUPLED])) [FIELD_DECODERS]"""

let EXTENSION_TEMPLATE = """
type [NAME] with[ENCODER][DECODER]"""

let BUILT_IN_TYPES =
    Set ["string"; "int"; "float"; "decimal"]

let log x = Browser.console.log(x)

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

let (|Matches|) (regex: Regex) txt =
    regex.Matches(txt)
    |> Seq.cast<Match>
    |> Seq.toList

let (|Groups|_|) (m: Match) =
    if not m.Success then
        None
    else
        let gs =
            m.Groups |> Seq.cast<Group>
            |> Seq.choose (fun g -> if g.Success then Some g.Value else None)
            |> Seq.toList
        Some(List.head gs, List.skip 1 gs)

let (|MatchGroups|_|) pattern (txt: string) =
    match Regex.Match(txt, pattern) with
    | Groups gs -> Some gs
    | _ -> None

let (|Lines|) f txt =
    Regex.Split(txt, LINE)
    |> Array.choose (fun line ->
        cleanComments line |> Option.bind f)
    |> Array.toList

let (|List|Option|Unknown|) = function
    | MatchGroups @"(\w+) (\w+)" (_, [typ; generic]) as txt ->
        match generic with
        | "list" -> List typ
        | "option" -> Option typ
        | _ -> Unknown txt
    | txt -> Unknown txt

let splitByColon (txt: string) =
    match txt.Split(':') with
    | [|part1; part2|] -> Some(part1.Trim(), part2.Trim())
    | _ -> None

let handleUnionCase line =
    match UNION_CASE.Match(line) with
    | Groups(_, [name]) -> Some(name, [])
    | Groups(_, [name; fields]) ->
        let fields =
            fields.Split('*') |> Array.map (fun field ->
                match splitByColon field with
                | Some(_, fieldType) -> fieldType
                | None -> field.Trim())
        Some(name, Array.toList fields)
    | _ -> None

let findRecords = function
    | Matches RECORD ms ->
        ms |> List.choose (function
            | Groups(_, [name; Lines splitByColon keyValue]) -> Some(name, keyValue)
            | _ -> None)

let findUnions = function
    | Matches UNION ms ->
        ms |> List.choose (function
            | Groups(_, [name; Lines handleUnionCase cases]) -> Some(name, cases)
            | _ -> None)

let printDecoderCall typ =
    let printType typ =
        if BUILT_IN_TYPES.Contains typ
        then "Decode." + typ
        else typ + ".Decoder"
    match typ with
    | List typ -> sprintf "(Decode.list %s)" (printType typ)
    // TODO: Make the field optional?
    | Option typ -> sprintf "(Decode.option %s)" (printType typ)
    | Unknown typ -> printType typ

let printEncoderCall typ =
    let printType typ =
        if BUILT_IN_TYPES.Contains typ
        then "Encode." + typ
        else typ + ".Encode"
    match typ with
    | List typ -> sprintf "(Encode.list %s)" (printType typ)
    | Option typ -> sprintf "(Encode.option %s)" (printType typ)
    | Unknown typ -> printType typ

let printRecordDecoder name (fields: (string*string) list) =
    let fields =
        fields |> List.map (fun (name, typ) ->
            RECORD_FIELD_DECODER_TEMPLATE
                .Replace("[NAME]", name)
                .Replace("[TYPE]", printDecoderCall typ))
        |> String.concat ""
    RECORD_DECODER_TEMPLATE
        .Replace("[NAME]", name)
        .Replace("[FIELDS]", fields)

let printRecordCoder (coderTemplate: string) (fieldTemplate: string) name (fields: (string*string) list) =
    let fields =
        fields |> List.map (fun (name, typ) ->
            fieldTemplate
                .Replace("[NAME]", name)
                .Replace("[TYPE]", printDecoderCall typ))
        |> String.concat ""
    coderTemplate
        .Replace("[NAME]", name)
        .Replace("[FIELDS]", fields)

let printRecordExtension name (fields: (string*string) list) =
    EXTENSION_TEMPLATE
        .Replace("[NAME]", name)
        .Replace("[ENCODER]", printRecordCoder RECORD_ENCODER_TEMPLATE RECORD_FIELD_ENCODER_TEMPLATE name fields)
        .Replace("[DECODER]", printRecordCoder RECORD_DECODER_TEMPLATE RECORD_FIELD_DECODER_TEMPLATE name fields)


let printUnionEncoder name (cases: (string * string list) list) =
    let cases =
        cases |> List.map (fun (name, fieldTypes) ->
            let fields1 = (fieldTypes |> List.mapi (fun i _ ->
                "f" + string (i+1)) |> String.concat ", ")
            let fields1 =
                match fieldTypes with
                | [] -> ""
                | [_] -> " " + fields1
                | _ -> "(" + fields1 + ")"
            let fields2 =
                fieldTypes |> List.mapi (fun i t ->
                    "; " + printEncoderCall t + " f" + string (i+1))
                |> String.concat ""
            UNION_FIELD_ENCODER_TEMPLATE
                .Replace("[NAME]", name)
                .Replace("[FIELDS1]", fields1)
                .Replace("[FIELDS2]", fields2)
        ) |> String.concat ""
    UNION_ENCODER_TEMPLATE
        .Replace("[NAME]", name)
        .Replace("[CASES]", cases)

let printUnionExtension name cases =
    EXTENSION_TEMPLATE
        .Replace("[NAME]", name)
        .Replace("[ENCODER]", printUnionEncoder name cases)
        .Replace("[DECODER]", "")

[<EntryPoint>]
let run (args: string[]) =
    let filePath =
        Array.tryHead args
        |> Option.defaultValue (resolvePath "${entryDir}../temp/Sample.fs")
    let file =
        Node.fs.readFileSync filePath |> string
    printfn "%s" filePath
    printfn "%s" SEPARATOR
    for (name, cases) in findUnions file do
        printUnionExtension name cases |> log
    for (name, fields) in findRecords file do
        printRecordExtension name fields |> log
    printfn "%s" SEPARATOR
    0
