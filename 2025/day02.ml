#!/usr/bin/env ocaml

(* this code solves both parts but quite slow *)

let (--) i j = Seq.ints i |> Seq.take (j-i+1);;

let is_repeat s l sl = if l mod sl > 0 then false else
    let ss = String.sub s 0 sl in
    Seq.for_all2 (==) (String.to_seq s) (String.to_seq ss |> Seq.cycle);;

let is_invalid x = let s = string_of_int x in
  let l = String.length s in
  Seq.exists (is_repeat s l) (1 -- l/2);;

let is_invalid1 x = let s = string_of_int x in
  let l = String.length s in
  l mod 2 == 0 && is_repeat s l (l / 2);;

let f x = let s, e =  match String.split_on_char '-' x with
  | [s;e] -> int_of_string s, int_of_string e
  | _ -> Printf.eprintf "\"%s\" is not separated with -" x ; exit 1 in
  let i1 = (s -- e) |> Seq.filter is_invalid1 |> Seq.fold_left (+) 0 in
  let i2 = (s -- e) |> Seq.filter is_invalid |> Seq.fold_left (+) 0 in
  i1, i2;;

let p1, p2 = read_line ()
  |> String.split_on_char ','
  |> List.map f
  |> List.fold_left (fun (a, b) (x, y) -> (a + x, b + y)) (0, 0) in
Printf.printf "%d\n%d\n" p1 p2
