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

let last_before x k = let res = first_after x k in
  let num = string_of_int res
    |> Seq.repeat
    |> Seq.take k
    |> List.of_seq
    |> String.concat "" in
  if String.equal num x then res else res - 1;;

module IntSet = Set.Make(Int);;

let primes =
  let cached = ref [2] in
  fun x ->
    for i = List.hd !cached to x do
      if not @@ List.exists (fun f -> i mod f == 0) !cached then
        cached := i :: !cached;
    done;
    List.rev !cached;;

let f x = let s, e = match String.split_on_char '-' x with
  | [s;e] -> s, e;
  | _ -> Printf.eprintf "\"%s\" is not separated with -" x ; exit 1 in
  let p1, p2 = ref IntSet.empty, ref IntSet.empty in
  let f2 n =
    let start, stop = first_after s n, last_before e n in
    for i = start to stop do
      let s = string_of_int i in
      let c = Seq.repeat s |> Seq.take n |> List.of_seq |> String.concat "" in
      let v = int_of_string c in
      if n == 2 then
        p1 := IntSet.add v !p1;
      p2 := IntSet.add v !p2
    done in
  List.iter f2 (primes @@ String.length e);
  let r1 = IntSet.elements !p1 |> List.fold_left (+) 0 in
  let r2 = IntSet.elements !p2 |> List.fold_left (+) 0 in
  r1, r2;;

let r1, r2 = ref 0, ref 0 in
read_line ()
|> String.split_on_char ','
|> List.map f
|> List.iter (fun (v1, v2) -> r1 := !r1 + v1; r2 := !r2 + v2);
Printf.printf "%d\n%d\n" !r1 !r2
