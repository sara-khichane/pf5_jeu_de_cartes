open Types
open XpatLib
open Card

(* initialiser le depot avec des cartes dont le rank est 0*)
val depot_init : card list

val retirer_carte_colonnes : 'a list FArray.t -> 'a -> 'a list FArray.t
  

(* ajoute une carte sur la carte arrivee du coup et renvoit les colonnes*)
val ajouter_carte_colonnes : (cardnum * 'a) list FArray.t ->
    cardnum * 'a -> cardnum * 'a -> (cardnum * 'a) list FArray.t

(* ajout d'une carte au depot *)
(* enlève une carte des colonnes / registres *)
(* return une partie *)
val ajout_carte_depot : partie -> card -> partie

(*verifie si la carte peut etre mise au dépot*)
val carte_to_depot : partie -> cardnum * suit -> bool
  

(* Mise au depot des cartes :
- ces deux fonctions dépendent s'appellent l'une à l'autre
- on cherche les colonnes dont le bout contient des cartes qui peuvent etre mise au depot
- si une carte trouvée, on la rajoute au dépot et on reprend la recherche à partir de la colonne 0
- a la fin de la recherche des colonnes, on cherche dans le registre
- si une carte trouvée, on la rajoute au dépot et on reprend la recherche à partir de la colonne 0
- si aucune carte trouvée ni dans les colonnes ni dans les registres, on renvoie la partie
*)
val mise_au_depot_registre : partie -> partie
val mise_au_depot : partie -> partie

  