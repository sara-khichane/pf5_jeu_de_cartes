open Types
open XpatLib
open Card
open Registres
open Depot


(*=========================================================*)
(* gestion d'un fichier solution                           *)
(*=========================================================*)

(* Lecture d'un fichier solution *)
val lire_fichier : string -> string list

val split : string -> string list

(* Fonction qui :
- recupère les listes des carte et des arrivee du fichier solution
- concertit les numéro de cartes en type carte
- une arrivee vide correspond à la carte fictife (14, Trefle) correspondant au numéro 52
- une arrivee registre correspond à la carte fictife (0, Trefle) correspondant au numéro 53
*)
val get_coups : cardnum list -> cardnum list -> coup list -> coup list

(* Fonction qui :
- convertit la liste des coups du fichier solution en et liste de carte liste d'arrivée
- utilise get_coups pour convertir au bon type de carte
- renvoie la liste de coups à jouer dans la partie
*)
val file_to_list_coups : string -> coup list

(* Fonction qui : 
- convertit la liste des coups trouvé en un fichier solution
*)
val list_coup_to_file : string -> coup list -> unit 

