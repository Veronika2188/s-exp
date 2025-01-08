type sexp =
  | Atom of string
  | SList of sexp list

let rec string_of_sexp sexp =
  match sexp with
  | Atom v -> v
  | SList l -> "(" ^ String.concat "" (List.map string_of_sexp l) ^ ")"

let tokenize input =
  let input = Str.global_replace (Str.regexp "(") " ( " input in
  let input = Str.global_replace (Str.regexp ")") " ) " input in
  List.filter (fun s -> String.length s > 0) (String.split_on_char ' ' input)

let rec parse_tokens tokens =
  match tokens with
  | [] -> raise (Failure "Unexpected end of input")
  | token :: rest ->
    match token with
    | "(" ->
      let children, rest = parse_list rest in
      SList children, rest
    | ")" -> raise (Failure "Unmatched )")
    | _ -> Atom token, rest

and parse_list tokens =
  match tokens with
  | [] -> raise (Failure "Unmatched (")
  | ")" :: rest -> [], rest
  | _ ->
    let sexp, rest = parse_tokens tokens in
    let sexps, final_rest = parse_list rest in
    sexp :: sexps, final_rest

let parse input =
  let tokens = tokenize input in
  match parse_tokens tokens with
  | sexp, [] -> sexp
  | _ -> raise (Failure "Invalid S-expression")

let () =
  let input = "(+ 1 (* 2 3))" in
  let result = parse input in
  print_endline (string_of_sexp result)
