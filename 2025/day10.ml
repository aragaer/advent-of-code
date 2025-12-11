#!/usr/bin/env ocaml

#use "topfind";;
#require "glpk";;

let failwith msg = raise (Failure msg);;

let rec solve1 num buttons = match buttons with
  | first :: rest -> min (1 + (solve1 (Int.logxor num first) rest)) (solve1 num rest)
  | _ -> if num == 0 then 0 else Int.max_int - 1;;

let solve_ax_b_min_sum num_rows num_cols b_vector non_zero_entries =
  let prob = Glpk.new_problem () in
  Glpk.set_direction prob Glpk.Minimize;
  Glpk.set_class prob Glpk.Mixed_integer_prog;
  Glpk.set_message_level prob 0;

  Glpk.add_columns prob num_cols;
  for j = 0 to num_cols - 1 do
    Glpk.set_obj_coef prob j 1.0;
    Glpk.set_col_bounds prob j Glpk.Lower_bounded_var 0.0 infinity;
    Glpk.set_col_kind prob j Glpk.Integer_var;
  done;

  Glpk.add_rows prob num_rows;
  for i = 0 to num_rows - 1 do
    let b_i = b_vector.(i) in
    Glpk.set_row_bounds prob i Glpk.Fixed_var b_i b_i;
  done;

  Glpk.load_sparse_matrix prob @@ Array.of_list non_zero_entries;

  Glpk.simplex prob;
  Glpk.branch_and_bound prob;

  let get_int_sol index =
    Glpk.get_col_primal prob index |> int_of_float in
  Some (Array.init num_cols get_int_sol)

let solve2 joltage buttons =
  let num_rows = List.length joltage in
  let num_cols = List.length buttons in
  let b_vector = Array.of_list joltage |> Array.map Float.of_int in
  let non_zero_entries = List.mapi
      (fun i bs -> List.map (fun b -> ((b,i),1.0)) bs) buttons |> List.flatten in
  try
    let result = solve_ax_b_min_sum num_rows num_cols b_vector non_zero_entries in
    match result with
    | Some x -> Array.fold_left (+) 0 x
    | None -> failwith "Execution finished without an optimal solution."
  with
  | Glpk.No_primal_feasible_solution ->
    failwith "Could not solve the problem due to infeasibility."
  | e ->
    Printf.eprintf "An unexpected error occurred: %s\n" (Printexc.to_string e); exit 1;;

let parse_lights s = String.to_seq s
  |> Seq.map (fun c -> if c == '#' then 1 else 0)
  |> Seq.map2 ( * ) (Seq.iterate (( * ) 2) 1)
  |> Seq.fold_left (+) 0 in
let parse_digits s = s
  |> String.split_on_char ','
  |> List.map int_of_string in
let parse_buttons s = String.split_on_char ' ' s
  |> List.map (fun s -> let l = String.length s in
                String.sub s 1 (l - 2)
                |> parse_digits) in
let parse_buttons2 lb = lb
  |> List.map (fun b -> b
                |> List.map (Int.shift_left 1)
                |> List.fold_left (+) 0) in
let res1 = ref 0 in
let res2 = ref 0 in
try
  while true do
    let line = read_line () in
    let l, b, j = Scanf.sscanf line "[%[.#]] %[^{]{%[0-9,]}"
        (fun l b j -> (parse_lights l, parse_buttons (String.trim b), parse_digits j)) in
    res1 := !res1 + solve1 l (parse_buttons2 b);
    res2 := !res2 + solve2 j b;
  done;
with
| End_of_file -> Printf.printf "%d\n%d\n" !res1 !res2
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1;;
