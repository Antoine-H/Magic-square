(* A magic square is a square matrix such that the sum of the elements of each
   line, column and diagonal are equal AND all elements are different (that is,
   they range from 1 to n²) *)

open Batteries
open Matrix

(* Create a matrix of n*n with all elements in [1,n²] *)

let create_square n =
  Random.self_init ();
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
  split (shuffle_line (n*n)) n

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

let check_diags ls n =
     check_line (get_diago ls) n
  && check_line (get_adiago ls) n

(* Last constraint : Elements must be all different. *)

let all_diff c = (List.length (List.flatten c))
               = (List.length (List.sort_uniq (-) (List.concat c)))

(* In a magic square of size n, the elements of a line must sum up to
   (n^3+n)/2. *)

let magic_constant c =
  let k = List.length c in
  (Int.pow k 3 + k) / 2

let sum_line l =
  let n = magic_constant l in
  abs ((List.fold_left (+) 0 l) - n)

(* Checks all the constraints at once. *)

let is_magic c =
  let n = magic_constant c in
     check_lines c n
  && check_cols c n
  && check_diags c n
  && all_diff c

(* The cost is the sum of the differences of the sum of each line and n.
   that is |sum line - n|*)

let cost_lines c =
  let rec sum_lines = function
    | []      -> []
    | l :: ls -> sum_line l :: sum_lines ls in
  List.fold_left (+) 0 (sum_lines c)

let cost_cols c = cost_lines (transpose c)

let cost_diago c = sum_line (get_diago c)

let cost_adiago c = sum_line (get_adiago c)

let cost c = cost_lines c + cost_cols c + cost_diago c + cost_adiago c

(* First attempt at neighbours.
   A neighbour is a matrix that got either a line or a column permuted. *)

let neighbours c = permutations (transpose c) @ permutations c

(* Second attempt at neighbours.
   A neighbour is a matrix on which a line/column has been shifted one spot
   (cyclic). Rubik's cube way*)

let rec shift_n c n = match n with
  | 0 -> begin match c with
    | [] -> []
    | l :: ls -> [(shift l)] @ ls
    end
  | _ -> match c with
    | [] -> []
    | l :: ls -> l :: (shift_n ls (n-1))

let rec shift_square c =
  let n = List.length c in
  let rec fs c n = match n with
    | 0  -> []
    | _  -> (shift_n c (n-1)) :: fs c (n-1) in
  fs c n

let neighbours2 (c: int list list) =
    (shift_square c) @ (shift_square (transpose c))

(* Third attempt at neighbours.
   Generates all the permutations of a square. Overflows.*)

let rec neighbours3 (c: int list list) =
  let n = List.length c in
  let c = List.flatten c in
  let cs = permutations c in
  let rec split2 cs = match cs with
  | [] -> []
  | c :: cs -> split c n :: split2 cs
  in split2 cs

(* Fourth attempt at neighbours.
   Swaps two random elements in a square *)

let rec neighbours4 (cs : int list list) =
  Random.self_init ();
  let n = List.length cs in
  let swap u v n = match n with
    |x when x = u -> v
    |x when x = v -> u
    |_ -> n
  in
  let list_swap l u v = List.map (swap u v) l in
  let rec swapn cs m =
    if m > 0 then
      let tmp = List.flatten cs in
      split (list_swap tmp
               (List.nth tmp (Random.int n))
               (List.nth tmp (Random.int n))) n
      :: swapn cs (m-1)
    else []
  in swapn cs (n*n)

(* Select the neighbour that is the closest to a solution.
   Chooses at random if multiple candidates. *)

let random_element cs =
  let n = Random.self_init (); Random.int (List.length cs) in
  List.nth cs n

(* Gets a minimum cost square. *)

let rec min cs =
  let rec min_carre cs k = match cs with
    | []      -> k
    | c :: cs ->
      if ((cost c) < (cost k)) then
        min_carre cs c
      else
        min_carre cs k
  in min_carre cs (List.hd cs)

(* Gets all square whose cost are the same as the minimum square. *)

let rec all_min cs =
  let m = min cs in
  let rec all_m cs m = match cs with
  | [] -> []
  | c :: cs ->
    if (cost c) = (cost m) then
      c :: (all_m cs m)
    else
      all_m cs m
  in all_m cs m

(* Picks one square among the minimums. *)

let mini cs = random_element (all_min cs)

(* Prints a list of matrices *)

let rec printc = function
  | [] -> print []
  | c :: cs ->
    print c;
    print_endline "";
    print_int (cost c);
    print_endline "";
    printc cs

(* Solver. *)

let rec solve c =
  if is_magic c then
    print c
  else
    let n = (neighbours c) @ (neighbours2 c) @ (neighbours4 c) in
    let m = mini n in
    (*let m = randomelement n in*)
    print m; print_endline "------------";
    print_int (cost m); print_endline ""; print_endline "------------";
    solve m

