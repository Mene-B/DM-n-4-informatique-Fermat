let test () = 
    assert (1=1);
    print_string "Tous les tests ont réussi \n"

let main () = 
    if (Array.length Sys.argv < 2) then failwith "Veuillez rentrer un argument\n" else 
    if (Sys.argv.(1) = "test") then test () else 
    print_string Sys.argv.(1); print_string "\n\n"

let _ = main ()
