
open Card
open PArray
open FArray

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type game = Freecell | Seahaven | Midnight | Baker

type config = { mutable game : game; mutable seed: int; mutable mode: mode }

type coup = { 
  carte : card; 
  arrivee : card;
}
(*les colonnes sont des farray de liste*)
(*les registres sont des parray de cartes*)
(*le depot est une liste de cartes*)
(*le plateau est une structure contenant les colonnes, les registres et le depot*)
(*la partie est une structure contenant le plateau et la configuration (le jeu)*)
(*score est le nombre de cartes dans le dépôt*)
type plateau = { colonnes: card list FArray.t ; 
                registre : card PArray.t ; 
                depot : card list ; 
                liste_coup : coup list; 
                compteur_coup : int;
                score : int };;
module Histo_plateau =
Set.Make (struct
type t = plateau
let compare = compare_parties
end)
;;
type partie = {mutable config : config; mutable plateau : plateau; histo_plateau : Histo_plateau.t};;

type depot = card list;;