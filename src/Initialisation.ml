open Types
open XpatLib
open Card
open FArray
open PArray
open Registres
open Colonnes
open Depot
open List


let longueur_colonnes game = match game with
| Freecell -> 7
| Seahaven -> 5
| Midnight -> 3
| Baker -> 4
;;


let list_to_split_list_freecell (list:  card list ) game =
  let rec aux list acc taille_colonne compteur_colonne compteur_carte liste_finale = 
    if list = [] then if acc = [] then liste_finale else (liste_finale @ [ acc]) 
    else match compteur_carte with
    (* si rangée impaire, alors compteur colonne mod 2 = 1, sachant que taille doit etre egale à 7, 6+1 = 7*)
    | x when x = (taille_colonne + ((compteur_colonne +1) mod 2) -1 ) -> aux list [] taille_colonne (compteur_colonne + 1) 0 (liste_finale @ [acc])
    | x -> aux (List.tl list) ((List.hd list)::acc) taille_colonne compteur_colonne (compteur_carte + 1) liste_finale
	in aux list [] 7 0 0 [[]];;


(* peut etre optimisé pour enlever le @ List.rev ?*)
(*problème pour seahven et ses registres ? -> liste_permut pas vide *)
let list_to_split_list (list : card list ) game =
  let rec aux list acc taille_colonne compteur_colonne compteur_carte liste_finale = 
    if list = [] then if acc = [] then liste_finale else (liste_finale @ [ acc]) 
    else match compteur_carte with
    | x when x = (taille_colonne (*- 1*)) -> aux list [] taille_colonne (compteur_colonne + 1) 0 (liste_finale @ [acc])
    | x when game = Baker && fst(List.hd list) = 13 -> aux (List.tl list) (acc@[(List.hd list)]) taille_colonne compteur_colonne (compteur_carte + 1) liste_finale
    | x -> aux (List.tl list) ((List.hd list)::acc) taille_colonne compteur_colonne (compteur_carte + 1) liste_finale
  in if game = Freecell then list_to_split_list_freecell list game
  else aux list [] (longueur_colonnes game) 0 0 [[]];;
;;

(*remplie les colonnes avec les listes de cartes dans la liste l*)
let rec remplir_colonne ( list: card list list) (colonnes : card list FArray.t) n =
  (*print_string "\n n : ";
  print_int (n-2);
  print_string " ";
  if n = 0 then print_string "debut de remplir" else List.iter (fun x -> print_string (Card.to_string x)) (FArray.get colonnes (n-2)); *)
 match n with
  | n when n = (FArray.length colonnes ) ->  FArray.set colonnes (n-1) (List.hd list);
  | n ->   remplir_colonne (List.tl list) (FArray.set colonnes (n-1) (List.hd list)) (n+1);;

let plateau_init config liste_permut = {colonnes = remplir_colonne (list_to_split_list liste_permut config.game) (array_init config.game) (0); 
registre = init_registres config.game liste_permut; depot = depot_init; liste_coup = []; compteur_coup = 0; score = 0;}
;;
  

(*initlaliser la partie*)
let init_partie game seed mode liste_permut = 
  let config = {game = game; seed = seed; mode =  mode } in 
  {config = config; plateau = (plateau_init config liste_permut); histo_plateau = Histo_plateau.empty}
;;
