type prop =
  | True
  | False
  | Atom of string
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Implies of prop * prop
  | EX of prop
  | AX of prop
  | EF of prop
  | AF of prop
  | EG of prop
  | AG of prop
  | EU of prop * prop
  | AU of prop * prop

let rec to_string = function
  | True -> "true"
  | False -> "false"
  | Atom name -> name
  | Not p -> "!(" ^ to_string p ^ ")"
  | And (a,b) -> "(" ^ to_string a ^ " & " ^ to_string b ^ ")"
  | Or (a,b) -> "(" ^ to_string a ^ " | " ^ to_string b ^ ")"
  | Implies (a,b) -> "(" ^ to_string a ^ " -> " ^ to_string b ^ ")"
  | EX p -> "EX " ^ to_string p
  | AX p -> "AX " ^ to_string p
  | EF p -> "EF " ^ to_string p
  | AF p -> "AF " ^ to_string p
  | EG p -> "EG " ^ to_string p
  | AG p -> "AG " ^ to_string p
  | EU (a,b) -> "E[" ^ to_string a ^ " U " ^ to_string b ^ "]"
  | AU (a,b) -> "A[" ^ to_string a ^ " U " ^ to_string b ^ "]"

let rec atoms = function
  | True | False -> []
  | Atom a -> [a]
  | Not p | EX p | AX p | EF p | AF p | EG p | AG p -> atoms p
  | And (a,b) | Or (a,b) | Implies (a,b) | EU (a,b) | AU (a,b) ->
      atoms a @ atoms b

let rec negate = function
  | True -> False
  | False -> True
  | Not p -> p
  | And (a,b) -> Or (negate a, negate b)
  | Or (a,b) -> And (negate a, negate b)
  | Implies (a,b) -> And (a, negate b)
  | p -> Not p
