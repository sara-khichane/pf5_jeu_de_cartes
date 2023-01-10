open XpatLib
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

  (* let compare_parties p1 p2 = 
    (*si toute colonne de p1 est égale à une des colonne de p2*)
    let rec aux i j acc =
      if i = FArray.length p1.colonnes then if acc = false then 0 else 1 else
        if j = FArray.length p2.colonnes then aux (i+1) 0 false else
          let compar = List.compare
            (fun x y -> if fst(x) <> fst(x) then (fst(x) - fst(x)) else if num_of_suit(snd(x)) <> num_of_suit(snd(y)) then num_of_suit(snd(x)) - num_of_suit(snd(y)) else 0)
            (FArray.get p1.colonnes i) (FArray.get p2.colonnes j) in
          if compar = 0 then aux i (j+1) true
          else aux (i+1) 0 (acc && true)
    in aux 0 0 true
  ;; *)
  
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