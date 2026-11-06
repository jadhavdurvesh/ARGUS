open Formula

type token = Ident of string | LParen | RParen | Not | And | Or | Implies | EX | AX | EF | AF | EG | AG | EU | AU | True | False | End
exception Parse_error of string

let tokenize source =
  let n = String.length source in
  let rec scan i acc =
    if i >= n then List.rev (End::acc)
    else if source.[i] = ' ' || source.[i] = '\n' || source.[i] = '\t' then scan (i+1) acc
    else if i+1 < n && String.sub source i 2 = "->" then scan (i+2) (Implies::acc)
    else match source.[i] with
    | '(' -> scan (i+1) (LParen::acc)
    | ')' -> scan (i+1) (RParen::acc)
    | '!' -> scan (i+1) (Not::acc)
    | '&' -> scan (i+1) (And::acc)
    | '|' -> scan (i+1) (Or::acc)
    | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
      let j = ref (i+1) in
      while !j < n && ((let c=source.[!j] in (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_')) do incr j done;
      let word = String.sub source i (!j-i) in
      let token = match word with
        | "true" -> True | "false" -> False
        | "EX" -> EX | "AX" -> AX | "EF" -> EF | "AF" -> AF
        | "EG" -> EG | "AG" -> AG | "EU" -> EU | "AU" -> AU
        | _ -> Ident word in
      scan !j (token::acc)
    | c -> raise (Parse_error (Printf.sprintf "unexpected character '%c' at offset %d" c i))
  in scan 0 []

let parse source =
  let tokens = Array.of_list (tokenize source) in
  let pos = ref 0 in
  let peek () = tokens.(!pos) in
  let take () = let t=peek () in incr pos; t in
  let expect token =
    if peek () = token then ignore (take ())
    else raise (Parse_error "unexpected token") in
  let rec implication () =
    let left = disjunction () in
    if peek () = Implies then (ignore (take ()); Implies (left, implication ())) else left
  and disjunction () =
    let rec loop left = if peek () = Or then (ignore (take ()); loop (Formula.Or (left, conjunction ()))) else left in
    loop (conjunction ())
  and conjunction () =
    let rec loop left = if peek () = And then (ignore (take ()); loop (Formula.And (left, unary ()))) else left in
    loop (unary ())
  and unary () =
    match take () with
    | Not -> Formula.Not (unary ())
    | EX -> Formula.EX (unary ())
    | AX -> Formula.AX (unary ())
    | EF -> Formula.EF (unary ())
    | AF -> Formula.AF (unary ())
    | EG -> Formula.EG (unary ())
    | AG -> Formula.AG (unary ())
    | True -> Formula.True
    | False -> Formula.False
    | Ident name -> Formula.Atom name
    | LParen ->
      let first = implication () in
      (match peek () with
       | EU | AU as op ->
         ignore (take ());
         let second = implication () in
         expect RParen;
         if op = EU then Formula.EU (first,second) else Formula.AU (first,second)
       | _ -> expect RParen; first)
    | _ -> raise (Parse_error "expected proposition")
  in
  let result = implication () in
  if peek () <> End then raise (Parse_error "trailing input");
  result
