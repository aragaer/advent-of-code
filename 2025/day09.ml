#!/usr/bin/env ocaml

(* top-left-bottom-right *)
let mkrect (x1,y1) (x2,y2) = (max y1 y2,min x1 x2,min y1 y2,max x1 x2);;
let area rect =
  let (top,left,bottom,right) = rect in
  (top-bottom+1)*(right-left+1);;
let inside rect =
  let (top,left,bottom,right) = rect in
  (top-1,left+1,bottom+1,right-1);;

let intersects rect1 rect2 =
  let (t1,l1,b1,r1) = rect1 in
  let (t2,l2,b2,r2) = rect2 in
  t1 > b2 && t2 > b1 && l1 < r2 && l2 < r1;;

let solve data =
  let edges = List.map2 mkrect data @@ ((List.tl data) @ (List.take 1 data)) in
  let is_good rect = not @@ List.exists (intersects (inside rect)) edges in
  let candidates = List.mapi (fun i p1 -> List.drop i data
                               |> List.map (mkrect p1)) data
    |> List.concat
    |> List.sort (fun r1 r2 -> compare (area r2) (area r1)) in
  Printf.printf "%d\n%d\n"
    (List.hd candidates |> area)
    (candidates |> List.find is_good |> area);;

let l2t l = (List.nth l 0),(List.nth l 1) in
let tmp = ref [] in
try
  while true do
    let cell = read_line ()
      |> String.split_on_char ','
      |> List.map int_of_string in
    tmp := l2t cell :: !tmp;
  done;
with
| End_of_file -> solve !tmp
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1;;
