type SExp =
    | Atom of string
    | SList of SExp list

let rec stringOfSExp sexp =
    match sexp with
    | Atom v -> v
    | SList l -> "(" + String.concat "" (List.map stringOfSExp l) + ")"

let tokenize input =
    input.Replace("(", " ( ").Replace(")", " ) ").Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let rec parseTokens tokens =
    match tokens with
    | [] -> Error "Unexpected end of input"
    | token :: rest ->
        match token with
        | "(" ->
            let result = parseList rest
            match result with
            | Ok (children, rest) -> Ok (SList children, rest)
            | Error e -> Error e
        | ")" -> Error "Unmatched )"
        | _ -> Ok (Atom token, rest)
and parseList tokens =
    match tokens with
    | [] -> Error "Unmatched ("
    | ")" :: rest -> Ok ([], rest)
    | _ ->
        match parseTokens tokens with
        | Ok (sexp, rest) ->
            match parseList rest with
            | Ok (sexps, finalRest) -> Ok (sexp :: sexps, finalRest)
            | Error e -> Error e
        | Error e -> Error e

let parse input =
    let tokens = tokenize input
    match parseTokens tokens with
    | Ok (sexp, []) -> sexp
    | Ok _ -> failwith "Invalid S-expression"
    | Error e -> failwith e

[<EntryPoint>]
let main _ =
    let input = "(+ 1 (* 2 3))"
    let result = parse input
    printfn "%s" (stringOfSExp result)
    0
