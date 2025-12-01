#!/usr/bin/env ocaml

let res, res2 = ref 0, ref 0 in
let pos = ref 50 in
try
  while true do
    let dir, dist = Scanf.sscanf (read_line ()) "%c%d" (fun x y -> x, y) in
    let tail = dist mod 100 in
    let clicks = dist / 100 + match dir with
      | 'L' when !pos > 0 && !pos <= tail -> 1
      | 'R' when !pos + tail >= 100 -> 1
      | _ -> 0 in
    res2 := !res2 + clicks;
    pos := (!pos + 100 + if dir == 'L' then -tail else tail) mod 100;
    if !pos == 0 then res := !res + 1
  done
with
| End_of_file -> Printf.printf "%d\n%d\n" !res !res2
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1;;
