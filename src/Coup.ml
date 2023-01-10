open Types
open XpatLib
open Card
open Registres
open Depot

(*=========================================================*)
(* gestion des coups                                       *)
(*=========================================================*)

(*Affichage d'un coup*)
let coup_to_string coup =

  print_string "[ ";

  print_string (to_string (coup.carte));

  print_string " ; ";

  print_string (to_string (coup.arrivee));

  print_string " ]\n"
;;


(* Fonction qui vérifie si les deux cartes
ont deux couleurs différentes   
*)
let is_opposite_color card1 card2 = 
  match snd(card1) with
  | Trefle | Pique when snd(card2) =  Coeur || snd(card2) = Carreau -> true
  | Coeur | Carreau when  snd(card2) =  Trefle || snd(card2) = Pique -> true
  | _ -> false 
;;

(* Fonction qui vérifie si la première carte est bien
est bien inférieure de exactement 1 de la deuxème
la première et la deuxième carte sont le départ et l'arrivée d'un coup
*)
let bonnombre carte arrivee =
    if fst(arrivee) = (fst(carte) + 1) then true 
    else false
;;

(*Affichage d'un booléen*)
let print_bool b = 
  if b then print_string "true" else print_string "false"
;;

(*Vérifie si une colonne vide existe dans le tableau des colonnes
renvoie booléen
*)
let exists_colonne_vide colonnes = FArray.exists (fun x -> x = []) colonnes
;;

(*Vérifie si une colonne vide existe dans le tableau des colonnes
renvoie booléen
*)
let is_bout_colonne carte colonnes =
  FArray.exists (fun x -> if x = [] then false else (List.hd x = carte)) colonnes
;;

(* Vérifie si la carte existe dans un des registres
*)
let is_dans_registres carte registre =
  let rec aux i =
    if i = PArray.length registre then false
    else if (PArray.get registre i = carte) then true 
    else aux (i+1)
  in aux 0
;;

(* Validité du coup :
  - Fonction qui check si c'est possible de placer la carte carte sur arrivee
  - elle dépend des règles de chaque jeu
*)
let coup_valide partie carte arrivee =
  if (not(is_bout_colonne carte partie.plateau.colonnes) && not(is_dans_registres carte partie.plateau.registre)) then false else
    if fst(arrivee) = 14 then (*carte vide*)
      if not(exists_colonne_vide partie.plateau.colonnes) then false (*faire exists_colonne_vide*)
      else
        match partie.config.game with
        | Freecell -> true
        | Seahaven -> if fst(carte) = 13 then true else false
        | Midnight -> false
        | Baker-> false
    else 
      if fst(arrivee) = 0 then
        match partie.config.game with
        | Freecell -> 
          begin
            (*print_string "\nif exists registre vide : ";*)
            (*print_bool (registre_vide partie.plateau.registre);*)
            registre_vide partie.plateau.registre
          end
        | Seahaven -> registre_vide partie.plateau.registre
        | Midnight -> false
        | Baker -> false
      else
        match partie.config.game with
        | Freecell -> if (is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else 
          begin
            (*print_string "\nis_opposite_color / bon_nombre : "; 
              print_bool (is_opposite_color carte arrivee) ; 
              print_string " "; print_bool (bonnombre carte arrivee); 
              print_string "\n";*) false;
          end
        | Seahaven -> if not(is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else false
        | Midnight -> if not(is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else false
        | Baker -> if (bonnombre carte arrivee) then true else false
;;
 
(* Jeu d'un coup :

  - Fonction qui joue un coup en mettant à jour l'historique
  - un coup peut aller de registre à colonne, de colonne à colonne ou de colonne à registre
  - si le coup est invalide alors la même partie d'entrée est renvoyée
*)
let add_coup partie coup =
  if coup_valide partie coup.carte coup.arrivee then
    if fst(coup.arrivee) = 0 then
      let plateau = {colonnes = retirer_carte_colonnes partie.plateau.colonnes coup.carte; registre = ajout_registres partie.plateau.registre coup.carte; depot = partie.plateau.depot; liste_coup = coup :: partie.plateau.liste_coup; compteur_coup = partie.plateau.compteur_coup + 1; score = partie.plateau.score} in
      let partie = {partie with plateau = plateau; histo_plateau = Histo_plateau.add plateau partie.histo_plateau} in
      mise_au_depot partie
    else
      let plateau = {colonnes = ajouter_carte_colonnes (retirer_carte_colonnes partie.plateau.colonnes coup.carte) coup.carte coup.arrivee; depot = partie.plateau.depot; registre = (enlever_ifexists_carte_registre partie.plateau.registre coup.carte); liste_coup = coup :: partie.plateau.liste_coup; compteur_coup = partie.plateau.compteur_coup + 1; score = partie.plateau.score} in
      let partie = {partie with plateau = plateau;  histo_plateau = Histo_plateau.add plateau partie.histo_plateau} in
      mise_au_depot partie
  else
    begin
      print_string "Coup invalide :";
      coup_to_string coup;
      print_string "\n";
      partie
    end
;;

(* Affichage des coups d'une liste de coups *)
let print_liste_coups liste =
  match liste with
  | [] -> print_string "Liste de coups vide\n"
  | _ -> 
    print_string "\nCoups possibles : \n"; 
    List.iter (fun x -> coup_to_string x) liste
;;

