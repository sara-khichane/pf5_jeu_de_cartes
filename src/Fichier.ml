open Types
open XpatLib
open Card
open Registres
open Depot


(*=========================================================*)
(* gestion d'un fichier solution                           *)
(*=========================================================*)

(* Lecture d'un fichier solution *)
let lire_fichier filename = 
  let rec aux filename acc = 
    try 
      let x = input_line filename in 
      aux filename (x::acc)
    with End_of_file -> acc
  in List.rev (aux (open_in filename) [])
;;

let split x =
  String.split_on_char ' ' x
;;


(* Fonction qui :
- recupère les listes des carte et des arrivee du fichier solution
- concertit les numéro de cartes en type carte
- une arrivee vide correspond à la carte fictife (14, Trefle) correspondant au numéro 52
- une arrivee registre correspond à la carte fictife (0, Trefle) correspondant au numéro 53
*)
let rec get_coups l_carte l_arrivee acc =
  match l_carte with
  | [] -> List.rev acc
  | x::xs -> (*print_string "\ncoup en int :\n";print_string "x : "; print_int x; print_string "\ny : "; print_int (List.hd l_arrivee); print_newline(); *)
  let y = List.hd l_arrivee in 

    if (x = 53) then (*registre*)
        if (y = 53) then let acc = {carte = (0, Trefle); arrivee = (0, Trefle)}::acc in
              get_coups xs (List.tl l_arrivee) acc
        else 
          if (y = 52) then let acc = {carte = (0, Trefle); arrivee = (14, Trefle)}::acc in
              get_coups xs (List.tl l_arrivee) acc
          else
          let acc = {carte = (0, Trefle); arrivee = of_num(y)}::acc in
                get_coups xs (List.tl l_arrivee) acc

    else if (x = 52) then (*carte est vide*)
      if (y = 53) then let acc = {carte = (14, Trefle); arrivee = (0, Trefle)}::acc in (*arrivee registre*)
            get_coups xs (List.tl l_arrivee) acc
      else if (y = 52) then let acc = {carte = (14, Trefle); arrivee = (14, Trefle)}::acc in (*arrivee vide*)
            get_coups xs (List.tl l_arrivee) acc
      else
      let acc = {carte = (14, Trefle); arrivee = of_num(y)}::acc in (*arrivee normale*)
            get_coups xs (List.tl l_arrivee) acc

    else (*carte normale*)
      if (y = 53) then let acc = {carte = of_num(x); arrivee = (0, Trefle)}::acc in (*arrivee registre*)
            (* print_string "--------->arrivee registre\n"; *)
            get_coups xs (List.tl l_arrivee) acc
      else 
        if (y = 52) then let acc = {carte = of_num(x); arrivee = (14, Trefle)}::acc in (*arrivee vide*)
            (* print_string "--------->arrivee vide\n"; *)
            get_coups xs (List.tl l_arrivee) acc
        else
        let acc = {carte = of_num(x); arrivee = of_num(y)}::acc in (*arrivee normale*)
              get_coups xs (List.tl l_arrivee) acc
;;

(*
- convertit la liste des coups du fichier solution en et liste de carte liste d'arrivée
- utilise get_coups pour convertir au bon type de carte
- renvoie la liste de coups à jouer dans la partie
*)
let file_to_list_coups filename =
  let l = lire_fichier filename in
  (* List.map (fun x -> print_string x; print_newline()) l;; *)
  if (List.length l) = 0 then []
  else
    let l1 = List.map (fun x -> split x) l in

    let l_carte = List.map (fun x -> List.hd x) l1 in
    let l_carte = List.map (fun x -> if x="T" then 53 else (*registre c'est 53*) (*carte vide c'est 52*)
      begin 
        if x="V" then 52 
        else int_of_string x
      end
      ) l_carte 
    in
    let l_arrivee = List.map (fun x -> List.nth x 1) l1 in
    let l_arrivee = List.map (fun x -> if x="T" then 53 else 
      begin
        if x="V" then 52 
        else int_of_string x
      end ) l_arrivee in

    let liste_coup = get_coups l_carte l_arrivee [] in 

  liste_coup
;;

(*
- convertit la liste des coups trouvé en un fichier solution
*)
let list_coup_to_file file_name liste_coup =
  let file = open_out file_name in
  (*la liste une liste de de coups qui a une carte et une arrivée*)
  List.iter (fun x -> 
    if (fst(x.carte) = 0) then 
      if (fst(x.arrivee) = 0) then output_string file "T T\n"
      else if (fst(x.arrivee) = 14) then output_string file "T V\n"
      else 
        begin
          output_string file "T "; 
          output_string file (string_of_int(Card.to_num x.arrivee));
          output_string file "\n";
        end
    
    else if (fst(x.carte) = 14) then
      if (fst(x.arrivee) = 0) then output_string file "V T\n"
      else if (fst(x.arrivee) = 14) then output_string file "V V\n"
      else 
        begin
          output_string file "T "; 
          output_string file (string_of_int(Card.to_num x.arrivee));
          output_string file "\n";
        end
    
    else
      if (fst(x.arrivee) = 0) then
        begin 
          output_string file (string_of_int(Card.to_num x.carte));
          output_string file " T\n";
        end
      else if (fst(x.arrivee) = 14) then
        begin
          output_string file (string_of_int(Card.to_num x.carte));
          output_string file " V\n";
        end
      else 
        begin
          output_string file (string_of_int(Card.to_num x.carte));
          output_string file " ";
          output_string file (string_of_int(Card.to_num x.arrivee));
          output_string file "\n";
        end
  ) (List.rev liste_coup);
  close_out file
;;
