(* Matrix manipulation toolss used to solve the magic square problem.
   Assuming square matrices only. *)

(* Splits a list into a list of lists of size n. *)

let split xs size =
  let (_, r, rs) =
    List.fold_left (fun (csize, ys, zss) elt ->
        if csize = 0 then (size - 1, [elt], zss @ [ys])
        else (csize - 1, ys @ [elt], zss))
             (size, [], []) xs
  in
  rs @ [r]

(* Transposes a matrix. *)

let rec transpose = function
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

(* Returns the diagonal of a matrix. *)

let get_diago ls =
  let rec get_diag ls i = match ls with
    | []      -> []
    | l :: ls -> List.nth l i :: get_diag ls (i+1)
  in get_diag ls 0

(* Returns the anti-diagonal of a matrix. *)

let get_adiago ls =
  let rec get_diag ls i = match ls with
    | []      -> []
    | l :: ls -> List.nth l i :: get_diag ls (i-1)
  in get_diag ls ((List.length ls)-1)

(* Generates all the permutations of elements in a list. *)

let rec permutations l =
  let rm x l = List.filter ((<>) x) l in
  match l with
  | []    -> []
  | x::[] -> [[x]]
  | l -> List.fold_left
           (fun acc x ->
              acc @ List.map (fun p -> x::p) (permutations (rm x l)))
           [] l

(* Shifts elements one spot to the left. *)

let shift = fun thelist ->
  let r = List.rev thelist
  in (List.hd r) :: (List.rev (List.tl r))

let shift2 l = shift (shift l)

(* Shifts elements one spot to the right. *)

let shiftR ls = List.tl ls @ [List.hd ls]

(* Prints a matrix. *)

let print c =
  let print_line l = List.iter (Printf.printf "%d ") l in
  List.iter (fun l -> print_line l; print_endline "") c

