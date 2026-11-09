open Formula
open Model

type result = { satisfying : IntSet.t; holds_initially : bool; witness : int list option; counterexample : int list option }

let find_path model starts targets allowed =
  let frontier = Queue.create () in
  let parents = ref IntMap.empty in
  let visited = ref IntSet.empty in
  IntSet.iter (fun id ->
    if IntSet.mem id allowed && not (IntSet.mem id !visited) then (
      visited := IntSet.add id !visited;
      parents := IntMap.add id (-1) !parents;
      Queue.add id frontier
    )
  ) starts;
  let rebuild target =
    let rec loop current acc =
      match IntMap.find_opt current !parents with
      | None -> acc
      | Some (-1) -> current :: acc
      | Some parent -> loop parent (current :: acc)
    in
    loop target []
  in
  let rec search () =
    if Queue.is_empty frontier then None
    else
      let id = Queue.take frontier in
      if IntSet.mem id targets then Some (rebuild id)
      else
        let successors =
          match state model id with
          | Some s -> IntSet.inter s.successors allowed
          | None -> IntSet.empty
        in
        IntSet.iter (fun next ->
          if not (IntSet.mem next !visited) then (
            visited := IntSet.add next !visited;
            parents := IntMap.add next id !parents;
            Queue.add next frontier
          )
        ) successors;
        search ()
  in
  search ()

let evaluate model formula =
  let universe = all_states model in
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
    in loop seed
  and until allowed target predecessor =
    let rec loop current =
      let next = IntSet.union target (IntSet.inter allowed (predecessor current)) in
      if IntSet.equal current next then current else loop next
    in loop target
  in
  let satisfying = eval formula in
  let holds_initially = IntSet.subset model.initial satisfying in
  let witness, counterexample = match formula with
    | EF p ->
      (find_path model model.initial (eval p) universe, None)
    | EU (a,b) ->
      (find_path model model.initial (eval b) (IntSet.union (eval a) (eval b)), None)
    | AG p ->
      (None, find_path model model.initial (IntSet.diff universe (eval p)) universe)
    | _ -> (None, None)
  in
  { satisfying; holds_initially; witness; counterexample }

let state_list set = IntSet.elements set
