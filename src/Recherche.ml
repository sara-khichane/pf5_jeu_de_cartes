open Types
open XpatLib
open Card
open Registres
open Depot
open Coup


(*=========================================================*)
(* recherche de coups                                      *)
(*=========================================================*)


(* ajout_coup_possible_registre : 
  renvoie la liste des coups possibles des colonnes vers les registres*)
let rec ajout_coup_possible_registre partie (acc : coup list) i = (*print_string "ajout possible de coup registre\n";*)
  if i = FArray.length partie.plateau.colonnes 
  then acc
  else if List.length (FArray.get partie.plateau.colonnes i) = 0 
    then ajout_coup_possible_registre partie acc (i+1)
    else let carte = List.hd (FArray.get partie.plateau.colonnes i) 
  in ajout_coup_possible_registre partie ({carte = carte; arrivee =  (0, Trefle)}::acc) (i+1)
;;


(* recherche_coup_registre_vers_col : 
  renvoie la liste des coups possibles des registres vers les colonnes*)
let rec recherche_coup_registre_vers_col partie acc i = (*print_string "recherche registre\n";*)
  let rec coup_registre_aux partie acc i j = 
    if i = PArray.length partie.plateau.registre then acc
    else if j = FArray.length partie.plateau.colonnes then recherche_coup_registre_vers_col partie acc (i+1)
    else if i <> j then
      let carte = (PArray.get partie.plateau.registre i) in
      if FArray.get partie.plateau.colonnes j = [] then coup_registre_aux partie ({carte = carte; arrivee = (14, Trefle)}::acc) i (j+1) else 
      let arrivee = List.hd (FArray.get partie.plateau.colonnes j) in
      if (coup_valide partie carte arrivee) then
        coup_registre_aux partie ({carte = carte; arrivee = arrivee} :: acc) i (j+1)
      else coup_registre_aux partie acc i (j+1)
    else coup_registre_aux partie acc i (j+1)
  in coup_registre_aux partie acc i 0
;;

(* recherche_coup_colonnes : 
  renvoie la liste des coups possibles des colonnes vers les colonnes (aka si une carte peut aller sur une autre)*)
let rec recherche_coup_colonnes partie acc i = (*print_string "recherche colonne\n";*)
  let rec coup_col_aux partie acc i j = 
    if i = FArray.length partie.plateau.colonnes then acc
    else if j = FArray.length partie.plateau.colonnes then recherche_coup_colonnes partie acc (i+1)
    else if FArray.get partie.plateau.colonnes i = [] then recherche_coup_colonnes partie acc (i+1)
    else if i <> j then
        let carte = List.hd (FArray.get partie.plateau.colonnes i) in
        if FArray.get partie.plateau.colonnes j = []
          then coup_col_aux partie ({carte = carte; arrivee = (14, Trefle)}::acc) i (j+1)
          else 
        let arrivee = List.hd (FArray.get partie.plateau.colonnes j) in
        if (coup_valide partie carte arrivee) then
        coup_col_aux partie ({carte = carte; arrivee = arrivee}::acc) i (j+1)
      else coup_col_aux partie acc i (j+1)
    else coup_col_aux partie acc i (j+1)
    in coup_col_aux partie acc i 0
;;
   

(*recherche_coup_possibles_registre : 
  verifie si une carte peut aller au registre (aka si il existe un registre vide, si oui, ajoute ce coup à la liste de coup)*)
let recherche_coup_possibles_registre (partie : partie) acc =
  if registre_vide partie.plateau.registre 
    then ajout_coup_possible_registre partie acc 0
    else acc
;;

(* recherche_coup_registre_colonnes : 
  renvoie la liste des coups possibles des registres vers les colonnes*)
let recherche_coup_registre_colonnes partie acc = 
  if PArray.exists (fun x -> x <> (0, Trefle)) partie.plateau.registre 
    then recherche_coup_registre_vers_col partie acc 0
    else []
;;

(* recherche_coup_possibles : 
  renvoie tous les coups possibles d'une partie*)
let recherche_coup_possibles partie =  (recherche_coup_possibles_registre partie (recherche_coup_colonnes  partie (recherche_coup_registre_colonnes partie []) 0)) 
;;
