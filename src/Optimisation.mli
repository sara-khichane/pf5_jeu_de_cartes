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
val best_score_coup : coup list -> partie -> coup

(*supprime un coup d'une liste*)
val remove_coup_liste_coup : 'a list -> 'a -> 'a list

(*renvoie la colonne d'une carte*)
val colonne_carte : card -> partie -> cardnum

(*optimisation 1 : si carte vient d'une colonne de 1 elem vers colonne vide*)
val carte_seule_to_vide : coup -> partie -> bool

(*optimisation 2 : si on a plusieurs colonnes vides, pas besoin de considérer le déplacement d'une carte vers toutes ces colonnes vides, le faire vers la première suffira.*)
(*on supprime donc les doublons dans la liste*)
val remove_doublons : 'a list -> 'a list

(*optimisation 3 : Comme en plus ces parties ont un usage contraint des colonnes vides, alors une longue séquence de cartes décroissantes de même couleur ne pourra plus être déplacée, et ne pourra évoluer que via une mise au dépôt. Par "longue", on entend ici (n+2) cartes au moins si l'on a n registres. Dans cette même colonne, si enfin une "petite" carte de la même couleur se trouve bloquée quelque part sous la "longue" séquence, alors la mise au dépôt de ces cartes sera toujours impossible. On pourra donc chercher en sommet de colonnes de telles "longues séquences bloquées", et supprimer de la recherche les états qui en contiennent, car ils sont insolubles.*)
val longue_sequence_bloquee_mo : (cardnum * suit) list -> partie -> bool
val longue_sequence_bloquee_st : (cardnum * suit) list -> partie -> bool

(*supprime les coups qui ne sont pas optimisés*)
val existe_longue_sequence_bloquee : partie -> bool

(*supprime les coups qui ne sont pas optimisés*)
val optimisation_list : coup list -> partie -> coup list

(*Renvoie la liste de coups optimisés après la recherche*)
val list_coup_optimise : partie -> coup list
