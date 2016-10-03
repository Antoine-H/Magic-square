open Search
open Matrix

let rec printc = function
  | [] -> print []
  | c :: cs ->
    print c;
    print_endline "";
    print_int (cost c);
    print_endline "";
    printc cs

let main =
  if Array.length Sys.argv <> 2 then
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <Size of square>")
  else
    let n = int_of_string(Sys.argv.(1)) in
    let carre = create_square n in
    Printf.printf "%B distance %d\n" (is_magic carre) (cost carre);
    print carre;
    print_endline "";
    solve carre

