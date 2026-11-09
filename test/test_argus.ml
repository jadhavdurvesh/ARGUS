let state id labels successors = Model.make_state ~labels id successors
let model = Model.create ~require_total:false
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


let () =
  let open Model in
  let partial = Model.create ~require_total:false
    ~states:[state 10 ["start"] [11]; state 11 ["finish"] []]
    ~initial:(IntSet.singleton 10)
  in
  check "deadlock detection" (IntSet.singleton 11) (deadlocks partial);
  check "non-total model accepted by default" false (is_total partial);
  let totalized = with_deadlock_self_loops partial in
  check "explicit self-loop totalization" true (is_total totalized);
  check "totalization preserves state IDs" (ids partial) (ids totalized);
  let rejected =
    try
      ignore (Model.create ~require_total:true
        ~states:[state 0 ["terminal"] []]
        ~initial:(IntSet.singleton 0));
      false
    with Invalid_argument message ->
      String.length message > 0
  in
  check "strict totality rejects deadlock" true rejected

let () =
  let open Formula in
  let manual_model = Model.create ~require_total:false
    ~states:[
      state 20 ["manual"] [21];
      state 21 ["manual"] [21]
    ]
    ~initial:(Model.IntSet.singleton 20)
  in
  let precedence = Checker.evaluate manual_model
    (AG (Implies (Atom "manual", AX (Atom "manual"))))
  in
  check "manual mode remains manual in every successor" true precedence.holds_initially;
  let unsafe_model = Model.create ~require_total:false
    ~states:[state 30 ["idle"] [31]; state 31 ["critical_thermal"; "moving"] [31]]
    ~initial:(Model.IntSet.singleton 30)
  in
  let blocked = Checker.evaluate unsafe_model
    (AG (Not (And (Atom "critical_thermal", Atom "moving"))))
  in
  check "thermal movement invariant exposes counterexample" false blocked.holds_initially;
  check "thermal invariant counterexample path" (Some [30;31]) blocked.counterexample
