#!/usr/bin/env ocaml

let flip shape = Array.to_list shape |> List.rev |> Array.of_list;;
let rotate shape =
  let h = Array.length shape in
  let w = Array.length shape.(0) in
  Array.init_matrix w h (fun i j -> shape.(w-j-1).(i));;
let perms shape =
  let f = flip shape in
  let r = rotate shape in
  let rf = rotate f in
  let r2 = rotate r in
  let r2f = rotate rf in
  let r3 = rotate r2 in
  let r3f = rotate r2f in
  List.to_seq [shape;r;r2;r3;f;rf;r2f;r3f];;

let all_shapes = ref []
let get_shape i = List.nth !all_shapes i;;

type area = {
  w: int;
  h: int;
  area: int array array;
}

let c2c v = if v == 1 then '#' else '.'

let dump area =
  Array.iter (fun row ->
      Array.to_seq row |> Seq.map c2c
      |> String.of_seq |> print_endline) area

let can_put_row to_fit to_fit_into pos =
  let s1 = Array.to_seq to_fit in
  let s2 = Array.to_seq to_fit_into |> Seq.drop pos in
  Seq.for_all2 (fun a b -> a == 0 || b == 0) s1 s2;;

let put shape area x y =
  let narea = Array.init_matrix area.h area.w (fun i j -> area.area.(i).(j)) in
  Array.iteri (fun i row ->
      Array.iteri (fun j v -> if shape.(i).(j) == 1 then
                      narea.(y+i).(x+j) <- 1) row) shape;
  {w=area.w;
   h=area.h;
   area=narea;};;

let can_put shape area x y =
  let sh = Array.length shape in
  let sw = Array.length shape.(0) in
  if area.w < sw + x || area.h < sh + y then false else begin
    let s1 = Array.to_seq shape in
    let s2 = Array.to_seq area.area |> Seq.drop y in
    Seq.for_all2 (fun r1 r2 -> can_put_row r1 r2 x) s1 s2
  end;;

let without pos arr = let narr = Array.copy arr in narr.(pos) <- narr.(pos) - 1; narr

let rec try_fit_at shapes area x y =
  Array.for_all ((==) 0) shapes ||
  if y >= area.h then false else begin
    let (nx, ny) = if x == area.w - 1 then (0, y+1) else (x+1,y) in
    if ny > y && Array.for_all ((==) 0) area.area.(y) then false else
    (Array.to_seq shapes
     |> Seq.zip (Seq.ints 0)
     |> Seq.exists (fun (i,c) -> c > 0 &&
                                 (Seq.exists (fun perm -> (can_put perm area x y) &&
                                                          (try_fit_at (without i shapes)
                                                             (put perm area x y)
                                                             nx ny)) (get_shape i)))) ||
    (try_fit_at shapes area nx ny)
  end

let shape_area shape_id =
  get_shape shape_id
  |> Seq.uncons
  |> Option.get
  |> fst
  |> Array.map (Array.fold_left (+) 0)
  |> Array.fold_left (+) 0;;

let shape_bounds shape_id =
  let shape = get_shape shape_id
    |> Seq.uncons
    |> Option.get
    |> fst in
  (Array.length shape.(0), Array.length shape);;

let upper_bound_check shapes region =
  let bounds = Array.to_seq shapes
    |> Seq.mapi (fun i count -> (count, shape_bounds i)) in
  let maxw = Seq.map snd bounds |> Seq.map fst |> Seq.fold_left max 0 in
  let maxh = Seq.map snd bounds |> Seq.map snd |> Seq.fold_left max 0 in
  let cellw = region.w / maxw in
  let cellh = region.h / maxh in
  cellw * cellh >= Array.fold_left (+) 0 shapes;;

let lower_bound_check shapes region =
  let area = region.w * region.h in
  let total_shape_area = Array.to_seq shapes
    |> Seq.mapi (fun i count -> count * shape_area i)
    |> Seq.fold_left (+) 0 in
  area >= total_shape_area;;

let try_fit shapes area =
  if upper_bound_check shapes area then
    true
  else if not @@ lower_bound_check shapes area then
    false
  else (* actual attempt at fitting stuff *)
    try_fit_at shapes area 0 0;;

let solve shapes regions =
  all_shapes := List.rev shapes;
  regions
  |> List.mapi (fun i (w,h,shapes) ->
      let area = {w=w; h=h; area=Array.make_matrix h w 0} in
      if try_fit shapes area then 1 else 0)
  |> List.fold_left (+) 0
  |> Printf.printf "%d\n";;

let shapes = ref [] in
let regions = ref [] in
let parse_region line =
  let s = String.split_on_char ':' line in
  let ps = List.nth s 1 |> String.trim |> String.split_on_char ' '
    |> List.map int_of_string |> Array.of_list in
  let a = List.hd s |> String.split_on_char 'x' |> List.map int_of_string in
  let x, y = (List.hd a, List.nth a 1) in
  regions := (min x y, max x y, ps) :: !regions in
let read_shape () =
  let finished = ref false in
  let shape = ref [] in
  while not !finished do
    let line = read_line () |> String.trim in
    if String.equal String.empty line then
      finished := true
    else
      shape := (String.to_seq line
                |> Seq.map (fun c -> if c == '#' then 1 else 0)) :: !shape
  done;
  shapes := (List.map Array.of_seq !shape
             |> Array.of_list |> perms) :: !shapes in
try
  while true do
    let line = read_line () in
    if String.contains_from line 0 'x' then
      parse_region line
    else
      read_shape ();
  done;
with
| End_of_file -> solve !shapes !regions
| e -> Printf.eprintf "Error: %s\n" (Printexc.to_string e); exit 1;;
