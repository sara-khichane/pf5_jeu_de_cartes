open Types
open XpatLib
open Card
open Registres
open Depot
open Coup
open Recherche

(*=========================================================*)
(* Optimisations                                           *)
(*=========================================================*)

(*Choisit le coup qui va donner la partie au score le plus élevé*)
let best_score_coup liste_coup partie = 
  let partie = mise_au_depot partie in
  let rec aux liste_coup partie best_coup best_score = 
    match liste_coup with
    | [] -> best_coup
    | x::xs -> 
      let tmp_partie = add_coup partie x in
      if tmp_partie.plateau.score > best_score then aux xs partie x tmp_partie.plateau.score
      else aux xs partie best_coup best_score
  in
  aux liste_coup partie (List.hd liste_coup) 0
;;

(*supprime un coup d'une liste*)
let remove_coup_liste_coup liste_coup coup = 
  let rec aux liste_coup acc = 
    match liste_coup with
    | [] -> acc
    | x::xs -> if x = coup then aux xs acc else aux xs (x::acc)
  in aux liste_coup []


(*renvoie la colonne d'une carte*)
let colonne_carte carte partie = 
  let rec aux i = 
    if i = FArray.length partie.plateau.colonnes then -1
    else if List.mem carte (FArray.get partie.plateau.colonnes i) then i
    else aux (i+1)
  in aux 0
;;

(*optimisation 1 : si carte vient d'une colonne de 1 elem vers colonne vide*)
let carte_seule_to_vide coup partie = 
  if ((fst(coup.arrivee)) = 14) && (List.length (FArray.get partie.plateau.colonnes (colonne_carte coup.carte partie)) = 1) then true
  else false
;;

(*optimisation 2 : si on a plusieurs colonnes vides, pas besoin de considérer le déplacement d'une carte vers toutes ces colonnes vides, le faire vers la première suffira.*)
(*on supprime donc les doublons dans la liste*)
let rec remove_doublons liste_coup = 
  match liste_coup with
  | [] -> []
  | x::xs -> if List.mem x xs then remove_doublons xs else x::(remove_doublons xs)
;;

(*optimisation 3 : Comme en plus ces parties ont un usage contraint des colonnes vides, alors une longue séquence de cartes décroissantes de même couleur ne pourra plus être déplacée, et ne pourra évoluer que via une mise au dépôt. Par "longue", on entend ici (n+2) cartes au moins si l'on a n registres. Dans cette même colonne, si enfin une "petite" carte de la même couleur se trouve bloquée quelque part sous la "longue" séquence, alors la mise au dépôt de ces cartes sera toujours impossible. On pourra donc chercher en sommet de colonnes de telles "longues séquences bloquées", et supprimer de la recherche les états qui en contiennent, car ils sont insolubles.*)
let longue_sequence_bloquee_mo colonne partie = 
  if partie.config.game = Midnight then
    if List.length colonne < 3 then false
    else
      let rec aux colonne = 
        match colonne with
        | [] -> false
        | x::[] -> false
        | x1::x2::[] -> false
        | x1::x2::xs -> if (not(is_opposite_color x1 x2)) && (not(is_opposite_color x2 (List.hd xs))) && (fst x1 > fst x2) && (fst x2 > fst (List.hd xs)) then true else aux (x2::xs)
        in aux colonne
  else false
;;


let longue_sequence_bloquee_st colonne partie = 
  if partie.config.game = Seahaven then
    if List.length colonne < 6 then false
    else
      let rec aux colonne = 
        match colonne with
        | [] -> false
        | x::[] -> false
        | x1::x2::[] -> false
        | x1::x2::x3::[] -> false
        | x1::x2::x3::x4::[] -> false
        | x1::x2::x3::x4::x5::[] -> false
        | x1::x2::x3::x4::x5::xs -> if (not(is_opposite_color x1 x2)) && (not(is_opposite_color x2 x3)) && (not(is_opposite_color x3 x4)) && (not(is_opposite_color x4 x5)) && (not(is_opposite_color x5 (List.hd xs))) && (fst x1 > (fst x2) ) && (fst x2 > (fst x3) ) && (fst x3 > (fst x4) ) && (fst x4 > (fst x5) ) && (fst x5 > (fst (List.hd xs)))
            then true
          else aux xs
      in aux colonne
  else false
;;

(*vérifie si une longue séquence exite*)
let existe_longue_sequence_bloquee partie = 
  (* print_string "existe_longue_sequence_bloquee\n"; *)
  let rec aux i = 
    if i = FArray.length partie.plateau.colonnes then false
    else if longue_sequence_bloquee_mo (FArray.get partie.plateau.colonnes i) partie then true
    else if longue_sequence_bloquee_st (FArray.get partie.plateau.colonnes i) partie then true
    else aux (i+1) 
  in aux 0
;;

(*supprime les coups qui ne sont pas optimisés*)
let optimisation_list liste_coup partie = 
  let rec aux liste_coup partie acc = 
    match liste_coup with
    | [] -> remove_doublons acc
    | x::xs -> 
      begin
        (*si la carte provient d'une liste vide vers une colonne vide*)
        (*supprimer le coup*)
        if (carte_seule_to_vide x partie) || (coup_valide partie x.carte x.arrivee == false)
          then
            begin
              (* print_string "coup refusé : "; *)
              (* coup_to_string x; *)
              (* print_int (colonne_carte x.carte partie); print_string "--->colonne de 1 elem\n"; *)
              aux xs partie acc
            end
        else
          aux xs partie (x::acc)
      end
  in aux liste_coup partie []
;;

(*Renvoie la liste de coups optimisés après la recherche*)
let list_coup_optimise partie = 
  let liste_coup = recherche_coup_possibles partie in
  let liste_coup = optimisation_list liste_coup partie in
  remove_doublons liste_coup
;;

