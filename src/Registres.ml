open Types
open XpatLib
open Card
open PArray
open FArray

let init_registres game (permut : card list ) =
  match game with
  | Freecell -> PArray.make 4 (0, Trefle)
  | Seahaven -> 
    begin
      let registres = PArray.make 4 (0, Trefle) in
      let registres = PArray.set registres 2 (List.nth permut (List.length permut -1)) in
      let registres = PArray.set registres 3 (List.nth permut (List.length permut - 2)) in
      registres
    end
  | Midnight -> PArray.make 0 (0, Trefle)
  | Baker -> PArray.make 0 (0, Trefle)
;;

(* retourne true s'il existe un registre vide dans la liste des registre du jeu *)
let registre_vide registres =
  let rec aux i =
    if i = PArray.length registres then false
    else if PArray.get registres i = (0, Trefle) then true
    else aux (i+1)
  in aux 0
;;

(* 
- ajoute une carte dans les registres et 
- trie le parray de registres de tel sorte que les vides soient en premiers --par optimisation--
*)
let ajout_registres registres carte =
  if registre_vide registres then
    let rec aux i =
      if PArray.get registres i = (0, Trefle) then
        PArray.set registres i ( carte)
      else
        aux (i+1)
    in
    aux 0
  else
    PArray.append registres (PArray.make 1 ( carte))
;;

(*si la carte se trouve dans les registres, on l'enlève*)
let enlever_ifexists_carte_registre registres carte =
  PArray.map (fun x -> if (x = carte) then (0, Trefle) else x) registres
;;
