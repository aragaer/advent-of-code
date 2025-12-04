#!/usr/bin/env ocaml

let around = [(1,1);(1,0);(1,-1);(0,1);(0,-1);(-1,1);(-1,0);(-1,-1)];;
let count_neigh m (x,y) =
  List.map (fun (dx, dy) -> (x + dx, y + dy)) around
  |> List.filter (Hashtbl.mem m)
  |> List.length;;

let pick m = Hashtbl.to_seq_keys m |> Seq.filter (fun k -> count_neigh m k < 4);;

let solve m =
  let total = Hashtbl.length m in
  let layer = ref (pick m) in
  Printf.printf "%d\n" (Seq.length !layer);
  while Seq.length !layer > 0 do
    Seq.iter (Hashtbl.remove m) !layer;
    layer := pick m
  done;
  Printf.printf "%d\n" (total - Hashtbl.length m);;

let data = Hashtbl.create 20000 in
try
  let f i =
    let row = String.to_seq (read_line ()) |> Seq.map ((==) '@') in
    let g (j, v) = if v then Hashtbl.add data (i, j) v in
    Seq.iter g @@ Seq.zip (Seq.ints 0) row
  in Seq.iter f @@ Seq.ints 0
with
| End_of_file -> solve data;
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1;;
