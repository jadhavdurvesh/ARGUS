open Formula
open Model

type result = { satisfying : IntSet.t; holds_initially : bool; witness : int list option; counterexample : int list option }

let evaluate model formula =
  let universe = all_states model in
  let get id = match state model id with Some s -> s | None -> failwith "invalid state reference" in
  let pre_exists set =
    IntMap.fold (fun id s acc ->
      if not (IntSet.is_empty (IntSet.inter s.successors set))
      then IntSet.add id acc else acc) model.states IntSet.empty in
  let pre_all set =
    IntMap.fold (fun id s acc ->
      if IntSet.for_all (fun next -> IntSet.mem next set) s.successors
      then IntSet.add id acc else acc) model.states IntSet.empty in
  let rec eval = function
    | True -> universe
    | False -> IntSet.empty
    | Atom name -> IntMap.fold (fun id s acc ->
        if StringSet.mem name s.labels then IntSet.add id acc else acc)
        model.states IntSet.empty
    | Not p -> IntSet.diff universe (eval p)
    | And (a,b) -> IntSet.inter (eval a) (eval b)
    | Or (a,b) -> IntSet.union (eval a) (eval b)
    | Implies (a,b) -> IntSet.union (IntSet.diff universe (eval a)) (eval b)
    | EX p -> pre_exists (eval p)
    | AX p -> pre_all (eval p)
    | EF p -> least_fix (eval p) pre_exists
    | AF p -> least_fix (eval p) pre_all
    | EG p -> greatest_fix (eval p) pre_exists
    | AG p -> greatest_fix (eval p) pre_all
    | EU (a,b) -> until (eval a) (eval b) pre_exists
    | AU (a,b) -> until (eval a) (eval b) pre_all
  and least_fix seed predecessor =
    let rec loop current =
      let next = IntSet.union seed (predecessor current) in
      if IntSet.equal current next then current else loop next
    in loop seed
  and greatest_fix seed predecessor =
    let rec loop current =
      let next = IntSet.inter seed (predecessor current) in
      if IntSet.equal current next then current else loop next
    in loop universe
  and until allowed target predecessor =
    let rec loop current =
      let next = IntSet.union target (IntSet.inter allowed (predecessor current)) in
      if IntSet.equal current next then current else loop next
    in loop target
  in
  let satisfying = eval formula in
  let holds_initially = IntSet.subset model.initial satisfying in
  { satisfying; holds_initially; witness = None; counterexample = None }

let state_list set = IntSet.elements set
