(*let print_plateau plateau = 
  print_string "\nSens de lecture des colonnes : -> \n";
  for i = 0 to FArray.length plateau.colonnes - 1 do
    print_string "[Col ";
    print_int (i);
    print_string "] : ";
    List.iter (fun x -> print_string (Card.to_string x); print_string" ") (List.rev (FArray.get plateau.colonnes (i)));
    print_newline();
  done;
  print_string "\nRegistre : ";
  PArray.iter (fun x -> print_string (Card.to_string x); print_string " " ) plateau.registre;

  print_string "\n\nDepot : ";
  List.iter (fun x -> print_string (Card.to_string x); print_string " " ) plateau.depot;;
*)