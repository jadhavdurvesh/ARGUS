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
    prerr_endline "Usage: argus <model-file> <formula>";
    prerr_endline "Model format: state <id> [label ...] -> <successor-id> ...";
    exit 2
  in
  if Array.length Sys.argv <> 3 then usage ();
  try
    let source = read_file Sys.argv.(1) in
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
    let model = Model.create ~states:!states ~initial:initial_set in
    let formula = Parser.parse Sys.argv.(2) in
    let result = Checker.evaluate model formula in
    Printf.printf "Formula: %s\n" (Formula.to_string formula);
    Printf.printf "Satisfying states: {%s}\n"
      (String.concat ", " (List.map string_of_int (Checker.state_list result.satisfying)));
    Printf.printf "Holds in every initial state: %b\n" result.holds_initially
  with
  | Parser.Parse_error message -> prerr_endline ("formula parse error: " ^ message); exit 1
  | Invalid_argument message -> prerr_endline ("model error: " ^ message); exit 1
  | Failure message -> prerr_endline ("input error: " ^ message); exit 1
