open XpatLib
open Card
open PArray
open FArray

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type game = Freecell | Seahaven | Midnight | Baker

type config = { mutable game : game; mutable seed: int; mutable mode: mode }

val config : config

type coup = { 
  carte : card; 
  arrivee : card;
}

(*score est le nombre de cartes dans le dépôt*)
type plateau = { colonnes: card list FArray.t ; 
                registre : card PArray.t ; 
                depot : card list ; 
                liste_coup : coup list; 
                compteur_coup : int;
                score : int };;

  (*regarde si chaque element de r1 existe dans r2*)
val compare_registres  : card PArray.t -> card PArray.t -> cardnum

val compare_parties : plateau -> plateau -> cardnum
  
module Histo_plateau : sig
  type t
  val compare : t -> t -> cardnum
  val add : plateau-> t -> t
  val empty : t
  val mem : plateau -> t -> bool
end

type partie = {mutable config : config; mutable plateau : plateau; histo_plateau : Histo_plateau.t};;

type depot = card list;; 

val print_plateau : plateau -> unit

val getgame : string -> game

val game_to_string : game -> string