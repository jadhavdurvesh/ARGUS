module IntSet = Set.Make (Int)
module StringSet = Set.Make (String)
module IntMap = Map.Make (Int)

type state = { id : int; labels : StringSet.t; successors : IntSet.t }
type t = { states : state IntMap.t; initial : IntSet.t }

let make_state ?(labels=[]) id successors =
  { id; labels = List.fold_left (fun acc x -> StringSet.add x acc) StringSet.empty labels;
    successors = List.fold_left (fun acc x -> IntSet.add x acc) IntSet.empty successors }

let create ~states ~initial =
  let table = List.fold_left (fun acc s ->
    if IntMap.mem s.id acc then invalid_arg ("duplicate state id " ^ string_of_int s.id);
    IntMap.add s.id s acc) IntMap.empty states in
  let known id = IntMap.mem id table in
  IntMap.iter (fun id s ->
    IntSet.iter (fun target ->
      if not (known target) then invalid_arg
        ("state " ^ string_of_int id ^ " references missing successor " ^ string_of_int target))
      s.successors) table;
  IntSet.iter (fun id ->
    if not (known id) then invalid_arg ("unknown initial state " ^ string_of_int id))
    initial;
  { states = table; initial }

let state model id = IntMap.find_opt id model.states
let ids model = IntMap.fold (fun id _ acc -> IntSet.add id acc) model.states IntSet.empty
let all_states model = ids model
let predecessors model =
  IntMap.fold (fun id s acc ->
    IntSet.fold (fun target map ->
      let old = match IntMap.find_opt target map with Some x -> x | None -> IntSet.empty in
      IntMap.add target (IntSet.add id old) map) s.successors acc)
    model.states IntMap.empty
