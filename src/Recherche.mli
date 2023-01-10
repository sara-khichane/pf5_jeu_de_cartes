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
val ajout_coup_possible_registre : partie -> coup list -> cardnum -> coup list

(* recherche_coup_registre_vers_col : 
  renvoie la liste des coups possibles des registres vers les colonnes*)
val recherche_coup_registre_vers_col : partie -> coup list -> cardnum -> coup list

(* recherche_coup_colonnes : 
  renvoie la liste des coups possibles des colonnes vers les colonnes (aka si une carte peut aller sur une autre)*)
val recherche_coup_colonnes : partie -> coup list -> cardnum -> coup list

(*recherche_coup_possibles_registre : 
  verifie si une carte peut aller au registre (aka si il existe un registre vide, si oui, ajoute ce coup à la liste de coup)*)
val recherche_coup_possibles_registre : partie -> coup list -> coup list

(* recherche_coup_registre_colonnes : 
  renvoie la liste des coups possibles des registres vers les colonnes*)
val recherche_coup_registre_colonnes : partie -> coup list -> coup list
