#!/usr/bin/env ocaml

let solve data =
  let res = ref 0 in
  let beam = String.index_from (List.hd data) 0 'S' in
  let beams = Array.make ((String.length (List.hd data)) + 1) 0 in
  Array.set beams beam 1;
  let iter line =
    let nbeams = Array.make (Array.length beams) 0 in
    String.iteri (fun i c -> if c == '^' then
                     (let count = beams.(i) in
                      Array.set beams i 0;
                      Array.set nbeams (i-1) (nbeams.(i-1) + count);
                      Array.set nbeams (i+1) (nbeams.(i+1) + count);
                      res := !res + if count == 0 then 0 else 1)) line;
    Array.mapi_inplace (fun i v -> v + nbeams.(i)) beams in
  List.iter iter @@ List.tl data;
  Printf.printf "%d\n%d\n" !res @@ Array.fold_left (+) 0 beams;;

let tmp = ref [] in
try
  while true do
    tmp := read_line () :: !tmp
  done;
with
| End_of_file -> solve @@ List.rev !tmp
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1;;
