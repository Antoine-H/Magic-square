(* A magic square is a square matrix such that the sum of the elements of each
   line, column and diagonal are equal AND all elements are different (that is,
   they range from 1 to n²) *)

open Batteries

(* Create a matrix of n*n with all elements in [1,n²] *)

let create_square n =
  let shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond
  in
  let rec populate_line n xs = match n with
    | 1 -> 1 :: xs
    | _ -> (populate_line (n-1) (n :: xs))
  in
  let shuffle_line n = shuffle (populate_line n [])
  in
  let split xs size =
    let (_, r, rs) =
      List.fold_left (fun (csize, ys, zss) elt ->
          if csize = 0 then (size - 1, [elt], zss @ [ys])
          else (csize - 1, ys @ [elt], zss))
               (size, [], []) xs
    in
    rs @ [r]
  in
  split (shuffle_line (n*n)) n

(* Transposes a matrix *)

let rec transpose = function
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

(* Check line constraint : the elements of a line must sum up to n *)

let check_line l n = (List.fold_left (+) 0 l) = n

(* Check the line constraint for each line *)

let rec check_lines ls n = match ls with
  | []      -> true
  | l :: ls -> check_lines ls n && check_line l n

(* The elements of a column must sum up to n.
   Reverses the matrix then uses the line constraint. *)

let check_cols ls n = check_lines (transpose ls) n

(* The elements of both diagonals must also sum up to n.
   Computes the diagonal then uses the line constraint. *)

let get_diago ls =
  let rec get_diag ls i = match ls with
    | []      -> []
    | l :: ls -> List.nth l i :: get_diag ls (i+1)
  in get_diag ls 0

let get_adiago ls =
  let rec get_diag ls i = match ls with
    | []      -> []
    | l :: ls -> List.nth l i :: get_diag ls (i-1)
  in get_diag ls ((List.length ls)-1)

let check_diags ls n =
     check_line (get_diago ls) n
  && check_line (get_adiago ls) n

(* Last constraint : Elements must be all different. *)

let all_diff c = (List.length (List.flatten c))
               = (List.length (List.sort_uniq (-) (List.concat c)))

(* In a magic square of size n, the elements of a line must sum up to
   (n^3+n)/2. *)

let get_sum c =
  let k = List.length c in
  (Int.pow k 3 + k) / 2

let sum_line l =
  let n = get_sum l in
  abs ((List.fold_left (+) 0 l) - n)

(* Checks all the constraints at once. *)

let is_magic c =
  let n = get_sum c in
     check_lines c n
  && check_cols c n
  && check_diags c n
  && all_diff c

(* The distance is the sum of the differences of the sum of each line and n
   that is |sum line - n|*)

let distance_lines c =
  let rec sum_lines = function
    | []      -> []
    | l :: ls -> sum_line l :: sum_lines ls in
  List.fold_left (+) 0 (sum_lines c)

let distance_cols c = distance_lines (transpose c)

let distance_diago c = sum_line (get_diago c)

let distance_adiago c = sum_line (get_adiago c)

let print c =
  let print_line l = List.iter (Printf.printf "%d ") l in
  List.iter (fun l -> print_line l; print_endline "") c

let distances c =
  distance_lines c + distance_cols c + distance_diago c + distance_adiago c

(* A neighbour is a matrix that got either a line or a column permuted.
   TODO This must not be enough since we are looping. *)

let neighbours c =
 let rm x l = List.filter ((<>) x) l in
  let rec permutations = function
    | []    -> []
    | x::[] -> [[x]]
    | l -> List.fold_left
        (fun acc x -> acc @ List.map (fun p -> x::p) (permutations (rm x l)))
        [] l
  in permutations (transpose c) @ permutations c

(* Second attempt at neighbours. A neighbour is a matrix on which a line/column
   has been shifted one spot (cyclic). Rubik's cube way*)

let flipper = fun thelist ->
  let r = List.rev thelist
  in (List.hd r) :: (List.rev (List.tl r))

let rec flipper_n c n = match n with
  | 0 -> begin match c with
    | [] -> []
    | l :: ls -> [(flipper l)] @ ls
    end
  | _ -> match c with
    | [] -> []
    | l :: ls -> l :: (flipper_n ls (n-1))

let rec flipper_square c =
  let n = List.length c in
  let rec fs c n = match n with
    | 0  -> []
    | _  -> (flipper_n c (n-1)) :: fs c (n-1) in
  fs c n

let neighbours2 (c: int list list) =
  (flipper_square c) @ (flipper_square (transpose c))

(* Select the neighbour that is the closest to a solution.
   Chooses at random if multiple candidates. *)

let randomelement cs =
  let n = Random.int (List.length cs) in
  List.nth cs n

let rec min cs =
  let rec min_carre cs k = match cs with
    | []      -> k
    | c :: cs ->
      if ((distances c) < (distances k)) then
        min_carre cs c
      else
        min_carre cs k
  in min_carre cs (List.hd cs)

let rec all_min cs =
  let m = min cs in
  let rec all_m cs m = match cs with
  | [] -> []
  | c :: cs ->
    if (distances c) = (distances m) then
      c :: (all_m cs m)
    else
      all_m cs m
  in all_m cs m

let mini cs = randomelement (all_min cs)

(* solver *)

let rec solve c =
  if is_magic c then
    print c
  else
    let n = (neighbours2 c) @ (neighbours c) in
    let m = mini n in
    (*let m = randomelement n in*)
    print m; print_endline "------------";
    print_int (distances m); print_endline ""; print_endline "------------";
    solve m

