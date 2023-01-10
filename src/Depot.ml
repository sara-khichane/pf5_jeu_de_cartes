open Types
open XpatLib
open Card
open Registres

(* initialiser le depot avec des cartes dont le rank est 0*)
let depot_init = [  (0, Trefle); (0, Pique); (0, Coeur); (0,Carreau) ] ;;

(* retire la carte du plateau et renvoit les colonnes*)
let retirer_carte_colonnes colonnes carte =
  FArray.map (fun x -> if x = [] then [] else if (List.hd x = carte) then List.tl x else x) colonnes;;

(* ajoute une carte sur la carte arrivee du coup et renvoit les colonnes*)
let ajouter_carte_colonnes colonnes carte arrivee = 
  if (fst(arrivee) = 14) then 
    begin
      (*mettre carte sur la premiere liste vide*)
      let rec aux i =
        if (FArray.get colonnes i) = [] then
          FArray.set colonnes i [carte]
        else
          aux (i+1)
      in aux 0
    end
  else
    FArray.map (fun x -> if x = [] then [] else if (List.hd x = arrivee) then carte::x else x) colonnes
;;

(* ajout d'une carte au depot *)
(* enlève une carte des colonnes / registres *)
(* return une partie *)
let ajout_carte_depot partie (carte : Card.card) = 
  (* print_string "---ajout carte depot----"; *)
  let depot = List.map (fun x -> if ((snd(x)) = (snd(carte))) then (fst(x)+1, snd(x)) else x) (partie.plateau.depot)  in
  let colonnes = FArray.map (fun x -> if (x <> [] && (List.hd x = carte)) then List.tl x else x) partie.plateau.colonnes in
  let registre = enlever_ifexists_carte_registre partie.plateau.registre carte in
  let plateau = { colonnes = colonnes; registre = registre; depot = depot; liste_coup = partie.plateau.liste_coup; compteur_coup = partie.plateau.compteur_coup; score = partie.plateau.score + 1} in
{config = partie.config ; plateau = plateau; histo_plateau = partie.histo_plateau}

(*verifie si la carte peut etre mise au dépot*)
let carte_to_depot partie carte = 
  let rec carte_to_depot_aux partie carte acc = match acc with
  | [] -> false
  | hd::tl when (snd(hd)) = (snd(carte)) && fst(hd) = (fst(carte) - 1) ->  (*print_newline(); print_string (Card.to_string(carte)); print_string "->la carte doit etre mise au depot\n";*) true
  | hd::tl -> carte_to_depot_aux partie carte tl
in carte_to_depot_aux partie carte partie.plateau.depot;;


(* Mise au depot des cartes :
- ces deux fonctions dépendent s'appellent l'une à l'autre
- on cherche les colonnes dont le bout contient des cartes qui peuvent etre mise au depot
- si une carte trouvée, on la rajoute au dépot et on reprend la recherche à partir de la colonne 0
- a la fin de la recherche des colonnes, on cherche dans le registre
- si une carte trouvée, on la rajoute au dépot et on reprend la recherche à partir de la colonne 0
- si aucune carte trouvée ni dans les colonnes ni dans les registres, on renvoie la partie
*)
let rec mise_au_depot_registre (partie : partie)= 
  let rec mise_au_depot_registre_aux partie acc = 
    if acc = PArray.length partie.plateau.registre then partie
    else
    if (carte_to_depot partie (PArray.get partie.plateau.registre acc)) then 
    begin 
      (* print_string "\ndu registre au depot ---> ";
      print_string (Card.to_string (PArray.get partie.plateau.registre acc));  *)
      let new_partie = mise_au_depot_registre_aux (ajout_carte_depot partie (PArray.get partie.plateau.registre acc)) 0
    in mise_au_depot new_partie
  end
    else mise_au_depot_registre_aux partie (acc+1)
  in mise_au_depot_registre_aux partie 0
  
and mise_au_depot partie = 
  let rec mise_au_depot_aux partie acc = 
    if acc = FArray.length partie.plateau.colonnes then 
      begin 
        (* print_string "\nfin verif colonnes pour depot\n";  *)
        (mise_au_depot_registre partie) 
      end
    else
      if (FArray.get partie.plateau.colonnes acc) = [] then 
        begin 
          (* print_newline(); 
          print_int acc;  *)
          mise_au_depot_aux partie (acc+1) 
        end
      else
      begin
        (* print_string "- la premiere de la colonne : ";
        print_string (Card.to_string (List.hd (FArray.get partie.plateau.colonnes acc))); *)
        if (carte_to_depot partie (List.hd (FArray.get partie.plateau.colonnes acc))) then 
          begin  
            (* print_newline(); 
            print_int acc; print_string "- des colonnes au depot ---> "; 
            print_string (Card.to_string (List.hd (FArray.get partie.plateau.colonnes acc)));  *)
            mise_au_depot_aux (ajout_carte_depot partie (List.hd (FArray.get partie.plateau.colonnes acc)) ) 0 
          end
        else 
          begin 
            (* print_newline(); 
            print_int acc;  *)
            mise_au_depot_aux partie (acc+1) 
          end
      end
  in
    mise_au_depot_aux partie 0
;;