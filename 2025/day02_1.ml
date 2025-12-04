#!/usr/bin/env ocaml

(* this code is slightly more complicated but it's 50 times faster *)

let (--) i j = Seq.ints i |> Seq.take (j-i+1);;

let chunks s chunk_len = let l = String.length s in
  let start = l mod chunk_len in
  let res = (0 -- (l / chunk_len - 1))
    |> Seq.map (fun x -> start + x * chunk_len)
    |> Seq.map (Fun.flip (String.sub s) chunk_len) in
  if start == 0 then res else Seq.cons (String.sub s 0 start) res;;

let carry over d c = if d + c > over then 1 else 0;;

let first_after x k = let l = String.length x in
  if l < k then 1 else
  let chunk_len = (l + k - 1)/k in
  if k*chunk_len - l >= chunk_len then 10 else
  let chunks = chunks x chunk_len |> Seq.map int_of_string in
  let first, rest = match Seq.uncons chunks with
    | Some (f, r) -> f, r
    | None -> Printf.eprintf "Expect at least 1 chunk" ; exit 1 in
  if String.length (string_of_int first) < chunk_len then
    "1" ^ (String.make (chunk_len - 1) '0') |> int_of_string
  else
    first + List.fold_right (carry first) (List.of_seq rest) 0;;

let last_before x k =
  let xn = int_of_string x |> (+) 1 |> string_of_int in
  (first_after xn k) - 1

let primes =
  let cached = ref [2] in
  fun x ->
    for i = (List.hd !cached) + 1 to x do
      if not @@ List.exists (fun f -> i mod f == 0) !cached then
        cached := i :: !cached;
    done;
    !cached;;

let f x = let s, e = match String.split_on_char '-' x with
  | [s;e] -> s, e;
  | _ -> Printf.eprintf "\"%s\" is not separated with -" x ; exit 1 in
  let p1, p2 = Hashtbl.create 1000, Hashtbl.create 1000 in
  let f2 n =
    for i = first_after s n to last_before e n do
      let s = string_of_int i in
      let c = Seq.repeat s |> Seq.take n |> List.of_seq |> String.concat "" in
      let v = int_of_string c in
      Hashtbl.add (if n == 2 then p1 else p2) v true;
    done in
  List.iter f2 (primes @@ String.length e);
  let r1 = Hashtbl.to_seq_keys p1 |> Seq.fold_left (+) 0 in
  let r2 = Hashtbl.to_seq_keys p2 |> Seq.fold_left (+) r1 in
  r1, r2;;

let r1, r2 = ref 0, ref 0 in
read_line ()
|> String.split_on_char ','
|> List.map f
|> List.iter (fun (v1, v2) -> r1 := !r1 + v1; r2 := !r2 + v2);
Printf.printf "%d\n%d\n" !r1 !r2
