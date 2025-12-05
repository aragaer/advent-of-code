#!/usr/bin/env ocaml

(* I forgot to save my original implementation and lost it.
   This one is more complicated but is blazing fast *)

let bin_search arr v =
  let a, b = ref 0, ref @@ (Array.length arr) - 1 in
  while !a <= !b do
    let mid = !a + (!b - !a) / 2 in
    if arr.(mid) == v then
      (a := mid; b := mid-1)
    else if arr.(mid) < v then
      a := mid + 1
    else
      b := mid - 1;
  done;
  !a;;

let data = ref @@ Array.make 0 0 in
let keep_reading = ref true in
while !keep_reading do
  let line = read_line () in
  if String.equal "" line then keep_reading := false else
  let s, e =  match String.split_on_char '-' line with
  | [s;e] -> (int_of_string s) - 1, int_of_string e
  | _ -> Printf.eprintf "\"%s\" is not separated with -" line ; exit 1 in
  let al = Array.length !data in
  let spos = bin_search !data s in
  let epos = bin_search !data e in
  let takes = 1 - spos mod 2 in
  let takee = 1 - epos mod 2 in
  let ndata = Array.make (spos + (al-epos) + takes + takee) (-1) in
  Array.set ndata (spos+takes-1) s;
  Array.set ndata (spos+takes) e;
  Array.blit (Array.sub !data 0 spos) 0 ndata 0 spos;
  Array.blit (Array.sub !data epos (al-epos)) 0 ndata (spos+takes+takee) (al-epos);
  data := ndata
done;
let res = ref 0 in
try
  while true do
    let v = int_of_string @@ read_line () in
    res := !res + (bin_search !data v) mod 2;
  done;
with
| End_of_file ->
  Printf.printf "%d\n" !res;
  Seq.iterate (( * ) (-1)) (-1)
  |> Seq.zip (Array.to_seq !data)
  |> List.of_seq
  |> List.map (fun (x,y) -> (x * y))
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n";
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e);
