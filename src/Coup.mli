open Types
open XpatLib
open Card
open Registres
open Depot


(*=========================================================*)
(* gestion des coups                                       *)
(*=========================================================*)

(*Affichage d'un coup*)
val coup_to_string : coup -> unit

(* Fonction qui vérifie si les deux cartes
ont deux couleurs différentes   
*)
val is_opposite_color : cardnum * suit -> cardnum * suit -> bool

(* Fonction qui vérifie si la première carte est bien
est bien inférieure de exactement 1 de la deuxème
la première et la deuxième carte sont le départ et l'arrivée d'un coup
*)
val bonnombre : cardnum * suit -> cardnum * suit -> bool

(*Affichage d'un booléen*)
val print_bool : bool -> unit

(*Vérifie si une colonne vide existe dans le tableau des colonnes
renvoie booléen
*)
val exists_colonne_vide : 'a list FArray.t -> bool

(*Vérifie si une colonne vide existe dans le tableau des colonnes
renvoie booléen
*)
val is_bout_colonne : 'a -> 'a list FArray.t -> bool

(* Vérifie si la carte existe dans un des registres
*)
val is_dans_registres : 'a -> 'a PArray.t -> bool

(* Validité du coup :
  - Fonction qui check si c'est possible de placer la carte carte sur arrivee
  - elle dépend des règles de chaque jeu
*)
val coup_valide : partie -> card -> cardnum * suit -> bool

(* Jeu d'un coup :

  - Fonction qui joue un coup en mettant à jour l'historique
  - un coup peut aller de registre à colonne, de colonne à colonne ou de colonne à registre
  - si le coup est invalide alors la même partie d'entrée est renvoyée
*)
val add_coup : partie -> coup -> partie

(* Affichage des coups d'une liste de coups *)
val print_liste_coups : coup list -> unit

