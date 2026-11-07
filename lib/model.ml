[@@@warning "-16"]

module IntSet = Set.Make (Int)
module StringSet = Set.Make (String)
module IntMap = Map.Make (Int)

type state = { id : int; labels : StringSet.t; successors : IntSet.t }
type t = { states : state IntMap.t; initial : IntSet.t }

let make_state ?(labels=[]) id successors =
  { id; labels = List.fold_left (fun acc x -> StringSet.add x acc) StringSet.empty labels;
    successors = List.fold_left (fun acc x -> IntSet.add x acc) IntSet.empty successors }

let create ?(require_total=false) ~states ~initial =
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
  let model = { states = table; initial } in
  if require_total then (
    let deadlocks =
      IntMap.fold (fun id s acc ->
        if IntSet.is_empty s.successors then id :: acc else acc)
        table []
      |> List.rev
    in
    if deadlocks <> [] then
      let ids = List.map string_of_int deadlocks in
      invalid_arg ("model contains deadlock states: " ^ String.concat ", " ids)
  );
  model

let state model id = IntMap.find_opt id model.states
let ids model = IntMap.fold (fun id _ acc -> IntSet.add id acc) model.states IntSet.empty
let all_states model = ids model
let predecessors model =
  IntMap.fold (fun id s acc ->
    IntSet.fold (fun target map ->
      let old = match IntMap.find_opt target map with Some x -> x | None -> IntSet.empty in
      IntMap.add target (IntSet.add id old) map) s.successors acc)
    model.states IntMap.empty

let deadlocks model =
  IntMap.fold (fun id s acc ->
    if IntSet.is_empty s.successors then IntSet.add id acc else acc)
    model.states IntSet.empty

let is_total model = IntSet.is_empty (deadlocks model)

(** Return a new model in which each deadlock state has an explicit self-loop.
    This is an opt-in transformation: callers must choose it deliberately
    because it changes the transition relation and therefore temporal results. *)
let with_deadlock_self_loops model =
  let states =
    IntMap.bindings model.states
    |> List.map (fun (id, s) ->
         if IntSet.is_empty s.successors then
           { s with successors = IntSet.singleton id }
         else s)
  in
  create ~states ~initial:model.initial
