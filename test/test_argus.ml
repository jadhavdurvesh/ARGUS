let state id labels successors = Model.make_state ~labels id successors
let model = Model.create
  ~states:[
    state 0 ["ready"] [1;2];
    state 1 ["safe"] [1];
    state 2 ["unsafe"] [2]
  ]
  ~initial:(Model.IntSet.singleton 0)

let check label expected actual =
  if expected <> actual then failwith ("failed: " ^ label)

let () =
  let open Formula in
  let open Model in
  let evaluate f = (Checker.evaluate model f).satisfying in
  check "atom" (IntSet.singleton 1) (evaluate (Atom "safe"));
  check "EX" (IntSet.singleton 0) (evaluate (EX (Atom "safe")));
  check "AX" IntSet.empty (evaluate (AX (Atom "safe")));
  check "EF" (IntSet.of_list [0;1]) (evaluate (EF (Atom "safe")));
  check "AG safe" IntSet.empty (evaluate (AG (Atom "safe")));
  check "EG safe" (IntSet.singleton 1) (evaluate (EG (Atom "safe")));
  check "existential until" (IntSet.of_list [0;1]) (evaluate (EU (Atom "ready",Atom "safe")));
  check "parser" (AG (Implies (Atom "ready", EF (Atom "safe"))))
    (Parser.parse "AG (ready -> EF safe)");
  print_endline "ARGUS tests passed"

let () =
  let open Formula in
  let reach = Checker.evaluate model (EF (Atom "safe")) in
  check "EF witness path" (Some [0;1]) reach.witness;
  let safety = Checker.evaluate model (AG (Atom "safe")) in
  check "AG counterexample prefix" (Some [0;2]) safety.counterexample
