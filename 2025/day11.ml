#!/usr/bin/env ocaml

let n2i = Hashtbl.create 1000
let id_of node = match Hashtbl.find_opt n2i node with
  | Some(i) -> i
  | None -> let i = Hashtbl.length n2i in
    Hashtbl.add n2i node i;
    i;;

let solve1 edge_data =
  let n = Hashtbl.length n2i in
  let children = Array.make n [] in
  List.iter (fun (p,c) -> children.(p) <- c) edge_data;
  let ids = Hashtbl.to_seq_values n2i |> List.of_seq in

  let counts = Array.make_matrix n n (-1) in
  let count_paths_to sink =
    Printf.printf "counting paths to %d\n%!" sink;
    counts.(sink).(sink) <- 1;
    let rec count_paths unvisited =
      match List.find_opt (fun n ->
          List.for_all (fun c -> counts.(sink).(c) <> -1) children.(n)) unvisited with
      | None -> ()
      | Some(node) ->
        counts.(sink).(node) <- List.map (Array.get counts.(sink)) children.(node)
          |> List.fold_left (+) 0;
        count_paths (List.filter ((<>) node) unvisited) in
    count_paths (List.filter ((<>) (sink)) ids) in

  let counted = ref [] in
  let num_paths node1 node2 =
    let t = id_of node2 in
    if not @@ List.mem t !counted then begin
      count_paths_to t;
      counted := t :: !counted;
    end;
    counts.(t).(id_of node1) in

  if Hashtbl.mem n2i "you" then
    Printf.printf "%d\n%!" @@ num_paths "you" "out";

  let num_route r =
    let rs = List.to_seq r in
    Seq.map2 num_paths rs (Seq.drop 1 rs)
    |> Seq.fold_left ( * ) 1 in

  if Hashtbl.mem n2i "svr" then
    Printf.printf "%d\n" @@ (max (num_route ["svr";"fft";"dac";"out"])
                               (num_route ["svr";"dac";"fft";"out"]));;


let edge_data = ref [] in
try
  while true do
    let line = read_line () in
    let parent, children =
      let s = String.split_on_char ':' line in
      (List.hd s,
       List.nth s 1 |> String.trim
       |> String.split_on_char ' ') in
    edge_data := (id_of parent, List.map id_of children) :: !edge_data;
  done;
with
| End_of_file -> solve1 !edge_data
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1;;
