open XpatLib
open Card
open PArray
open FArray

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type game = Freecell | Seahaven | Midnight | Baker

type config = { mutable game : game; mutable seed: int; mutable mode: mode }
    
let config = { game = Freecell; seed = 1; mode = Search "" }

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

  (*regarde si chaque element de r1 existe dans r2*)
let compare_registres r1 r2 = 
  let rec aux i =
    if i = PArray.length r1 then 0 else
      if PArray.exists (fun x -> x = PArray.get r1 i) r2
        then aux (i+1)
      else fst(PArray.get r1 i)
  in aux 0

let compare_parties p1 p2 =
  (* print_string "\np1 : \n";
  print_plateau p1;
  print_string "\np2 : \n";
  print_plateau p2; *)
  if not(p1.score = p2.score) then p1.score - p2.score else
  let rec aux i =
    if (i = FArray.length p1.colonnes) 
      then let compar_r =  compare_registres p1.registre p2.registre  in if compar_r = 0 then 0 else compar_r
      else
      let compar = List.compare 
    (fun x y -> if fst(x) <> fst(y) then (fst(x) - fst(y)) else if num_of_suit(snd(x)) <> num_of_suit(snd(y)) then num_of_suit(snd(x)) - num_of_suit(snd(y)) else 0)
      (FArray.get p1.colonnes i) (FArray.get p2.colonnes i) in
    if compar = 0 then aux (i+1)
    else compar
  in aux 0
;;

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
  
module Histo_plateau =
  Set.Make (struct
    type t = plateau
    let compare = compare_parties
    end)
  ;;



type partie = {mutable config : config; mutable plateau : plateau; histo_plateau : Histo_plateau.t};;

type depot = card list;;

let print_plateau plateau = 
  print_string "\nSens de lecture des colonnes : -> \n";
  for i = 0 to FArray.length plateau.colonnes - 1 do
    print_string "[Col ";
    print_int (i);
    print_string "] : ";
    List.iter (fun x -> print_string (Card.to_string x); print_string" ") (List.rev (FArray.get plateau.colonnes (i)));
    print_newline();
  done;
  print_string "\nRegistre : ";
  PArray.iter (fun x -> print_string (Card.to_string x); print_string " " ) plateau.registre;

  print_string "\n\nDepot : ";
  List.iter (fun x -> print_string (Card.to_string x); print_string " " ) plateau.depot;;


let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found

let game_to_string game = match game with
  | Freecell -> "FreeCell"
  | Seahaven -> "Seahaven Towers"
  | Midnight -> "Midnight Oil"
  | Baker -> "Baker's Dozen";;
 