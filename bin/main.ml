let read_file path =
  let channel = open_in path in
  Fun.protect ~finally:(fun () -> close_in channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let split_arrow words =
  let rec loop labels = function
    | [] -> (List.rev labels, [])
    | "->"::successors -> (List.rev labels, successors)
    | word::rest -> loop (word::labels) rest
  in
  loop [] words

let () =
  let usage () =
    prerr_endline "Usage: argus [--require-total | --totalize-deadlocks] <model-file> <formula>";
    prerr_endline "  --require-total       reject models containing states with no successors";
    prerr_endline "  --totalize-deadlocks  add explicit self-loops to deadlock states";
    prerr_endline "Model format: state <id> [label ...] -> <successor-id> ...";
    exit 2
  in
  let option, model_path, formula_text =
    match Array.to_list Sys.argv |> List.tl with
    | [path; formula] -> (None, path, formula)
    | [ (("--require-total" | "--totalize-deadlocks") as opt); path; formula] ->
        (Some opt, path, formula)
    | _ -> usage ()
  in
  try
    let source = read_file model_path in
    let lines = String.split_on_char '\n' source in
    let states = ref [] and initials = ref [] in
    List.iter (fun line ->
      let line = String.trim line in
      if line <> "" && line.[0] <> '#' then
        match String.split_on_char ' ' line |> List.filter ((<>) "") with
        | ["initial"; id] -> initials := int_of_string id :: !initials
        | "state"::id::rest ->
          let id = int_of_string id in
          let labels, successors = split_arrow rest in
          let successors = List.map int_of_string successors in
          states := Model.make_state ~labels id successors :: !states
        | _ -> failwith ("invalid model line: " ^ line)
    ) lines;
    let initial_set =
      List.fold_left (fun set id -> Model.IntSet.add id set)
        Model.IntSet.empty !initials
    in
    let model = Model.create
      ~require_total:(option = Some "--require-total")
      ~states:!states ~initial:initial_set in
    let model =
      if option = Some "--totalize-deadlocks"
      then Model.with_deadlock_self_loops model
      else model
    in
    let formula = Parser.parse formula_text in
    let result = Checker.evaluate model formula in
    Printf.printf "Formula: %s\n" (Formula.to_string formula);
    Printf.printf "Satisfying states: {%s}\n"
      (String.concat ", " (List.map string_of_int (Checker.state_list result.satisfying)));
    Printf.printf "Holds in every initial state: %b\n" result.holds_initially
  with
  | Parser.Parse_error message -> prerr_endline ("formula parse error: " ^ message); exit 1
  | Invalid_argument message -> prerr_endline ("model error: " ^ message); exit 1
  | Failure message -> prerr_endline ("input error: " ^ message); exit 1
