#!/usr/bin/env ocaml

let rec maximize enable digits =
  let len = List.length digits in
  let available = len - enable + 1 in
  let max_fst = List.fold_left max 0 @@ (List.take available digits) in
  if enable == 1 then [max_fst] else
  let rest = List.drop_while ((>) max_fst) digits |> List.tl in
  max_fst :: (maximize (enable - 1) rest);;

let eval line enable = line
  |> String.to_seq
  |> List.of_seq
  |> List.map Char.code
  |> maximize enable
  |> List.map Char.chr
  |> List.to_seq
  |> String.of_seq
  |> int_of_string;;

let res1, res2 = ref 0, ref 0 in
try
  while true do
    let line = read_line () in
    res1 := !res1 + (eval line 2);
    res2 := !res2 + (eval line 12);
  done
with
| End_of_file -> Printf.printf "%d\n%d\n" !res1 !res2
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1;;

