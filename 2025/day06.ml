#!/usr/bin/env ocaml

let is_empty s = String.trim s |> String.equal String.empty;;

let res1, res2 = ref 0, ref 0;;
let calc ch =
  let op_line = List.hd ch in
  let op_ch = op_line.[(String.length op_line) - 1] in
  let op, acc = match op_ch with
    | '+' -> ((+), 0)
    | '*' -> (( * ), 1)
    | x -> (Printf.eprintf "Error: got %c instead of * or +\n" x; exit 1) in
  let digits = String.sub op_line 0 ((String.length op_line) - 1) :: List.tl ch in
  res2 := digits
    |> List.map (fun l -> String.trim l |> int_of_string)
    |> List.fold_left op acc
    |> (+) !res2;
  res1 := digits
  |> List.to_seq
  |> Seq.map String.to_seq
  |> Seq.transpose
  |> Seq.map (fun l -> String.of_seq l |> String.trim)
  |> Seq.filter (fun s -> not @@ is_empty s)
  |> Seq.map int_of_string
  |> Seq.fold_left op acc
  |> (+) !res1;;

let solve tmp =
  let data = List.rev tmp
    |> List.to_seq
    |> Seq.map String.to_seq
    |> Seq.transpose
    |> Seq.map (fun l -> String.of_seq l) in
  let grp = ref [] in
  Seq.iter (fun l ->
      if is_empty l then
        (calc !grp; grp := [])
      else
        grp := !grp @ [l]) data;
  calc !grp;
  Printf.printf "%d\n%d\n" !res1 !res2;;

let tmp = ref [] in
try
  while true do
    tmp := read_line () :: !tmp
  done;
with
| End_of_file -> solve !tmp
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1;;
