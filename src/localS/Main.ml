open Carre

let rec printc = function
  | [] -> print []
  | c :: cs ->
    print c;
    print_endline "";
    print_int (distances c);
    print_endline "";
    printc cs

let main =
  let n = int_of_string(Sys.argv.(1)) in
  let carre = create_square n in
  Printf.printf "%B distance %d\n" (is_magic carre) (distances carre);
  print carre;
  print_endline "";
  solve carre

