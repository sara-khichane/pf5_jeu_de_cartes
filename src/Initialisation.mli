open Types
open XpatLib
open Card
open FArray
open PArray
open Registres
open Colonnes


val longueur_colonnes : game -> cardnum


val list_to_split_list_freecell : card list -> 'a -> card list list

(* peut etre optimisé pour enlever le @ List.rev ?*)
(*problème pour seahven et ses registres ? -> liste_permut pas vide *)
val list_to_split_list : card list -> game -> card list list

(*remplie les colonnes avec les listes de cartes dans la liste l*)
val remplir_colonne : card list list -> card list FArray.t -> cardnum -> card list FArray.t

val plateau_init : config -> card list -> plateau
