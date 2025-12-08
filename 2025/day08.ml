#!/usr/bin/env ocaml

module Dsu = struct
  (* DSU implementation taken from
   https://ssojet.com/data-structures/implement-disjoint-set-in-ocaml/ *)

  type disjoint_set = {
    parents : int array;
    ranks : int array;
  }

  let make_set n =
    { parents = Array.make n (-1); ranks = Array.make n 0 }

  let find ds element =
    let rec find_recursive current =
      if ds.parents.(current) < 0 then current
      else
        let root = find_recursive ds.parents.(current) in
        ds.parents.(current) <- root; (* Path compression *)
        root
    in
    find_recursive element

  let union ds elem1 elem2 =
    let root1 = find ds elem1 in
    let root2 = find ds elem2 in
    if root1 <> root2 then
      let rank1 = ds.ranks.(root1) in
      let rank2 = ds.ranks.(root2) in
      if rank1 < rank2 then
        ds.parents.(root1) <- root2
      else if rank1 > rank2 then
        ds.parents.(root2) <- root1
      else begin
        ds.parents.(root2) <- root1;
        ds.ranks.(root1) <- rank1 + 1
      end
end

let solve data =
  let nboxes = Array.length data in
  let steps = if nboxes == 20 then 10 else 1000 in
  let dsu = Dsu.make_set nboxes in
  let d2l =
    let tmp = ref [] in
    let d2 = List.fold_left2 (fun a x1 x2 -> a + (x1-x2)*(x1-x2)) 0 in
    for i = 0 to nboxes - 2 do
      for j = i + 1 to nboxes - 1 do
        tmp := (i,j,d2 data.(i) data.(j)) :: !tmp
      done
    done;
    List.sort (fun (i1,j1,d1) (i2,j2,d2) -> compare d1 d2) !tmp
    |> List.to_seq
    |> Queue.of_seq in
  let circuits = ref nboxes in
  let join_once () = begin
    let (i,j,d) = Queue.take d2l in
    if Dsu.find dsu i <> Dsu.find dsu j then begin
      circuits := !circuits - 1;
      if !circuits == 1 then
        Printf.printf "%d\n" ((List.hd data.(i)) * (List.hd data.(j)));
    end;
    Dsu.union dsu i j;
  end in
  for step = 1 to steps do
    join_once ()
  done;
  let sizes = Array.make nboxes 1 in
  for i = 0 to nboxes - 1 do
    let p = Dsu.find dsu i in
    if p <> i then
      sizes.(p) <- sizes.(p) + 1;
  done;
  Array.to_list sizes
  |> List.sort (fun s1 s2 -> compare s2 s1)
  |> List.take 3
  |> List.fold_left ( * ) 1
  |> Printf.printf "%d\n";
  while !circuits > 1 do join_once () done;;

let tmp = ref [] in
try
  while true do
    let box = read_line ()
      |> String.split_on_char ','
      |> List.map int_of_string in
    tmp := box :: !tmp;
  done;
with
| End_of_file -> solve @@ Array.of_list !tmp
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1;;
