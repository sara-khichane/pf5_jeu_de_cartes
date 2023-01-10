open Types
open XpatLib
open Card

(*=========================================================*)
(* affichage d'une partie                                  *)
(*=========================================================*)


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

let print_partie partie = 
  print_string "\nSens de lecture des colonnes : -> \n";
  for i = 0 to FArray.length partie.plateau.colonnes - 1 do
    print_string "[Col ";
    print_int (i);
    print_string "] : ";
    List.iter (fun x -> print_string (Card.to_string x); print_string" ") (List.rev (FArray.get partie.plateau.colonnes (i)));
    print_newline();
  done;
  print_string "\nRegistre : ";
  PArray.iter (fun x -> print_string (Card.to_string x); print_string " " ) partie.plateau.registre;

  print_string "\n\nDepot : ";
  List.iter (fun x -> print_string (Card.to_string x); print_string " " ) partie.plateau.depot;

  (*print_string "\n\nListe des coups joués : ";
  print_liste_coups partie.plateau.liste_coup;*)

  print_string "\n\nNombre de coups joués : ";
  print_int partie.plateau.compteur_coup;

;;