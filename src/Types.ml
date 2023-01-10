open XpatLib
open Card
open PArray
open FArray
let config = { game = Freecell; seed = 1; mode = Search "" }

(* 
                                (* Compare 2 colonnes *)
let compare_colonne c1 c2 = if (List.compare (fun x y -> if fst(x) <> fst(y) then (fst(x) - fst(y)) else if num_of_suit(snd(x)) <> num_of_suit(snd(y)) then num_of_suit(snd(x)) - num_of_suit(snd(y)) else 0)
(c1) (c2)) = 0 then true else false;;

let compare_parties p1 p2 = 
  (* pour toutes les colonnes de p1, on regarde si cette colonne existe dans p2 *)
if (FArray.for_all (fun x -> FArray.exists (fun y -> compare_colonne x y) p2.colonnes) p1.colonnes) = true then 0 else 
  let rec aux i =
    if i = FArray.length p1.colonnes then 0 else
      let compar = List.compare 
    (fun x y -> if fst(x) <> fst(y) then (fst(x) - fst(y)) else if num_of_suit(snd(x)) <> num_of_suit(snd(y)) then num_of_suit(snd(x)) - num_of_suit(snd(y)) else 0)
      (FArray.get p1.colonnes i) (FArray.get p2.colonnes i) in
    if compar = 0 then aux (i+1)
    else compar
  in aux 0
;; *)

    

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


