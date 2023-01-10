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

(*les colonnes sont des farray de liste*)
(*les registres sont des parray de cartes*)
(*le depot est une liste de cartes*)
(*le plateau est une structure contenant les colonnes, les registres et le depot*)
(*la partie est une structure contenant le plateau et la configuration (le jeu)*)

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
 
(*=========================================================*)
(* Structure de gestion des colonnes                       *)
(*=========================================================*)

(* initialise une FArray selon la partie en cours *)
let array_init game = let n = 
  match game with
  | Freecell -> 8
  | Midnight -> 18
  | Seahaven -> 10
  | Baker-> 13
  in FArray.make n [];;
;;

(*=========================================================*)
(* Structure de gestion des registres                      *)
(*=========================================================*)

(*
- fonction qui initialise les registres de chaque jeu
---> la carte de remplissage par défaut : (0, Trefle)
*)
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

(*=========================================================*)
(* Structure de gestion du depot                           *)
(*=========================================================*)

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

(*=========================================================*)
(* Init une partie                                         *)
(*=========================================================*)

let longueur_colonnes game = match game with
| Freecell -> 7
| Seahaven -> 5
| Midnight -> 3
| Baker -> 4
;;

let list_to_split_list_freecell (list:  card list ) game =
  let rec aux list acc taille_colonne compteur_colonne compteur_carte liste_finale = 
    if list = [] then if acc = [] then liste_finale else (liste_finale @ [ acc]) 
    else match compteur_carte with
    (* si rangée impaire, alors compteur colonne mod 2 = 1, sachant que taille doit etre egale à 7, 6+1 = 7*)
    | x when x = (taille_colonne + ((compteur_colonne +1) mod 2) -1 ) -> aux list [] taille_colonne (compteur_colonne + 1) 0 (liste_finale @ [acc])
    | x -> aux (List.tl list) ((List.hd list)::acc) taille_colonne compteur_colonne (compteur_carte + 1) liste_finale
	in aux list [] 7 0 0 [[]];;

(* peut etre optimisé pour enlever le @ List.rev ?*)
(*problème pour seahven et ses registres ? -> liste_permut pas vide *)
let list_to_split_list (list : card list ) game =
  let rec aux list acc taille_colonne compteur_colonne compteur_carte liste_finale = 
    if list = [] then if acc = [] then liste_finale else (liste_finale @ [ acc]) 
    else match compteur_carte with
    | x when x = (taille_colonne (*- 1*)) -> aux list [] taille_colonne (compteur_colonne + 1) 0 (liste_finale @ [acc])
    | x when game = Baker && fst(List.hd list) = 13 -> aux (List.tl list) (acc@[(List.hd list)]) taille_colonne compteur_colonne (compteur_carte + 1) liste_finale
    | x -> aux (List.tl list) ((List.hd list)::acc) taille_colonne compteur_colonne (compteur_carte + 1) liste_finale
  in if game = Freecell then list_to_split_list_freecell list game
  else aux list [] (longueur_colonnes game) 0 0 [[]];;
;;

(*remplie les colonnes avec les listes de cartes dans la liste l*)
let rec remplir_colonne ( list: card list list) colonnes n =
  (*print_string "\n n : ";
  print_int (n-2);
  print_string " ";
  if n = 0 then print_string "debut de remplir" else List.iter (fun x -> print_string (Card.to_string x)) (FArray.get colonnes (n-2)); *)
 match n with
  | n when n = (length colonnes ) ->  FArray.set colonnes (n-1) (List.hd list);
  | n ->   remplir_colonne (List.tl list) (FArray.set colonnes (n-1) (List.hd list)) (n+1);; (*AVANT : INSTRUCTION 1 ; 2*)
  (*SOLUTION A VOIR : let remplir_colonne2 = of_list l;; *)
(*let remplir_colonne list  = FArray.of_list list;;*)

let plateau_init config liste_permut = {colonnes = remplir_colonne (list_to_split_list liste_permut config.game) (array_init config.game) (0); 
registre = init_registres config.game liste_permut; depot = depot_init; liste_coup = []; compteur_coup = 0; score = 0;}
;;


(*=========================================================*)
(* gestion des coups                                       *)
(*=========================================================*)

let coup_to_string (coup : coup)= 
  print_string "[ ";
  print_string (to_string (coup.carte));
  print_string " ; ";
  print_string (to_string (coup.arrivee));
  print_string " ]\n"
;;

type histo_coup = coup list;; (*partie 2*)

let is_opposite_color card1 card2 = 
	match snd(card1) with
	| Trefle | Pique when snd(card2) =  Coeur || snd(card2) = Carreau -> true
	| Coeur | Carreau when  snd(card2) =  Trefle || snd(card2) = Pique -> true
	| _ -> false ;;

let bonnombre carte arrivee =
    if fst(arrivee) = (fst(carte) + 1) then true 
    else false

let print_bool b = 
  if b then print_string "true" else print_string "false";;

let exists_colonne_vide colonnes = FArray.exists (fun x -> x = []) colonnes;;

let is_bout_colonne carte colonnes =
  FArray.exists (fun x -> if x = [] then false else (List.hd x = carte)) colonnes;;

let is_dans_registres carte registre =
  let rec aux i =
    if i = PArray.length registre then false
    else if (PArray.get registre i = carte) then true 
    else aux (i+1)
  in aux 0;;

(*Fonction qui check si c'est possible de placer la carte carte sur arrivee*)
let coup_valide partie carte arrivee =
  if (not(is_bout_colonne carte partie.plateau.colonnes) && not(is_dans_registres carte partie.plateau.registre)) then false else
    if fst(arrivee) = 14 then (*carte vide*)
      if not(exists_colonne_vide partie.plateau.colonnes) then false (*faire exists_colonne_vide*)
      else
        match partie.config.game with
        | Freecell -> true
        | Seahaven -> if fst(carte) = 13 then true else false
        | Midnight -> false
        | Baker-> false
    else 
      if fst(arrivee) = 0 then
        match partie.config.game with
        | Freecell -> 
          begin
            (*print_string "\nif exists registre vide : ";*)
            (*print_bool (registre_vide partie.plateau.registre);*)
            registre_vide partie.plateau.registre
          end
        | Seahaven -> registre_vide partie.plateau.registre
        | Midnight -> false
        | Baker -> false
      else
        match partie.config.game with
        | Freecell -> if (is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else 
          begin
            (*print_string "\nis_opposite_color / bon_nombre : "; print_bool (is_opposite_color carte arrivee) ; print_string " "; print_bool (bonnombre carte arrivee); print_string "\n";*) false;
          end
        | Seahaven -> if not(is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else false
        | Midnight -> if not(is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else false
        | Baker -> if (bonnombre carte arrivee) then true else false
  ;;

(* let trouver_coup = failwith "TODO";; (*partie 2*) *)

(* let add_coup_history coup party = coup :: party.liste_coup;;  (*partie 2*)*)
 
let add_coup partie coup =
  if coup_valide partie coup.carte coup.arrivee then
    if fst(coup.arrivee) = 0 then
      let plateau = {colonnes = retirer_carte_colonnes partie.plateau.colonnes coup.carte; registre = ajout_registres partie.plateau.registre coup.carte; depot = partie.plateau.depot; liste_coup = coup :: partie.plateau.liste_coup; compteur_coup = partie.plateau.compteur_coup + 1; score = partie.plateau.score} in
      let partie = {partie with plateau = plateau; histo_plateau = Histo_plateau.add plateau partie.histo_plateau} in
      mise_au_depot partie
    else
      let plateau = {colonnes = ajouter_carte_colonnes (retirer_carte_colonnes partie.plateau.colonnes coup.carte) coup.carte coup.arrivee; depot = partie.plateau.depot; registre = (enlever_ifexists_carte_registre partie.plateau.registre coup.carte); liste_coup = coup :: partie.plateau.liste_coup; compteur_coup = partie.plateau.compteur_coup + 1; score = partie.plateau.score} in
      let partie = {partie with plateau = plateau;  histo_plateau = Histo_plateau.add plateau partie.histo_plateau} in
      mise_au_depot partie
  else
    begin
      print_string "Coup invalide :";
      coup_to_string coup;
      print_string "\n";
      partie
    end
;;
  

(*=========================================================*)
(* affichage d'une partie                                  *)
(*=========================================================*)

let list_coup_to_file file_name liste_coup =
  let file = open_out file_name in
  (*la liste une liste de de coups qui a une carte et une arrivée*)
  List.iter (fun x -> 
    if (fst(x.carte) = 0) then 
      if (fst(x.arrivee) = 0) then output_string file "T T\n"
      else if (fst(x.arrivee) = 14) then output_string file "T V\n"
      else 
        begin
          output_string file "T "; 
          output_string file (string_of_int(Card.to_num x.arrivee));
          output_string file "\n";
        end
    
    else if (fst(x.carte) = 14) then
      if (fst(x.arrivee) = 0) then output_string file "V T\n"
      else if (fst(x.arrivee) = 14) then output_string file "V V\n"
      else 
        begin
          output_string file "T "; 
          output_string file (string_of_int(Card.to_num x.arrivee));
          output_string file "\n";
        end
    
    else
      if (fst(x.arrivee) = 0) then
        begin 
          output_string file (string_of_int(Card.to_num x.carte));
          output_string file " T\n";
        end
      else if (fst(x.arrivee) = 14) then
        begin
          output_string file (string_of_int(Card.to_num x.carte));
          output_string file " V\n";
        end
      else 
        begin
          output_string file (string_of_int(Card.to_num x.carte));
          output_string file " ";
          output_string file (string_of_int(Card.to_num x.arrivee));
          output_string file "\n";
        end
  ) (List.rev liste_coup);
  close_out file
;;


let print_liste_coups liste = 
  match liste with
  | [] -> print_string "Liste de coups vide\n"
  | _ -> List.iter (fun x -> coup_to_string x) liste
;;

let print_partie partie = 
  print_string "\nSens de lecture des colonnes : -> \n";
  for i = 0 to FArray.length partie.plateau.colonnes - 1 do
    print_string "[Col ";
    print_int (i);
    print_string "] : ";
    List.iter (fun x -> print_string (Card.to_string x); print_string" ") (List.rev (FArray.get partie.plateau.colonnes (i)));
    print_newline();
  done;
  print_string "\nRegistre : ";
  PArray.iter (fun x -> print_string (Card.to_string x); print_string " " ) partie.plateau.registre;

  print_string "\n\nDepot : ";
  List.iter (fun x -> print_string (Card.to_string x); print_string " " ) partie.plateau.depot;

  (*print_string "\n\nListe des coups joués : ";
  print_liste_coups partie.plateau.liste_coup;*)

  print_string "\n\nNombre de coups joués : ";
  print_int partie.plateau.compteur_coup;

;;


(*=========================================================*)
(* déroulement de la partie                                *)
(*=========================================================*)

(*initlaliser la partie*)
let init_partie game seed mode liste_permut = 
  let config = {game = game; seed = seed; mode =  mode } in 
  {config = config; plateau = (plateau_init config liste_permut); histo_plateau = Histo_plateau.empty}
;;

(*jouer la partie jusqu'à terminaison*)
let rec jouer_partie partie liste_coup i =

  if (List.length liste_coup = 0) && (i=1) then
    begin
      print_string "Fichier solution vide !\n";
      print_string "ECHEC ";
      print_int i;
      exit 1;
    end
  else 
    if (List.length liste_coup = 0) && (i>1) then
      begin
        let partie = mise_au_depot(partie) in 
        print_string "\n-----------partie terminée-----------\n";
        print_partie partie;
        (*Permet de tester Histo_plateau*)
        (*Histo_plateau.iter (fun x -> print_plateau x; print_string "\n" ) partie.histo_plateau;*)
        if (*dans la liste de cartes du dépot, leur rank doit etre 13 --> des rois*)
          match partie.plateau.depot with
          | [] -> false
          | x::xs -> 
            let rec aux l =
              match l with
              | [] -> true
              | x::xs -> if (fst(x) = 13) then aux xs else false
            in aux partie.plateau.depot
          then 
            begin
              print_string "\nSUCCES\n";
              exit 0;
            end
          else
            begin
              print_string "\nECHEC ";
              print_int i;
              print_newline();
              exit 1;
            end
      end

else
  (*print liste_coups* avec coup_to_string*)
    print_string "\n\n-----------> Début du jeu du coup numéro ";
    print_int i;
    print_string " : ";
    coup_to_string (List.hd liste_coup);

  print_partie partie;
  print_newline();

  match liste_coup with
  | [] -> partie
  | x::xs -> 
    if not(coup_valide (partie) x.carte x.arrivee) 
    then
      begin
        print_newline();
        (coup_to_string x);
        print_string " -> Ce coup échoue ! ";
        print_string "\nECHEC ";
        print_int i;
        exit 1
      end
    else
      (* print_string "\nProchain coup : ";
      (coup_to_string x);
      print_string "\nPartie : \n"; *)
      (* print_string "\nbefore maj depot\n"; *)
      let partie = mise_au_depot (add_coup partie x) in
      (* print_string "\nafter maj depot\n"; *)
      jouer_partie partie xs (i+1) 
;;

(*=========================================================*)
(* gestion d'un fichier solution                           *)
(*=========================================================*)

let lire_fichier filename = 
  let rec aux filename acc = 
    try 
      let x = input_line filename in 
      aux filename (x::acc)
    with End_of_file -> acc
  in List.rev (aux (open_in filename) [])
;;

let split x =
  String.split_on_char ' ' x
;;

(*
- recupère les listes des carte et des arrivee du fichier solution
- concertit les numéro de cartes en type carte
- une arrivee vide correspond à la carte fictife (14, Trefle) correspondant au numéro 52
- une arrivee registre correspond à la carte fictife (0, Trefle) correspondant au numéro 53
*)
let rec get_coups l_carte l_arrivee acc =
  match l_carte with
  | [] -> List.rev acc
  | x::xs -> (*print_string "\ncoup en int :\n";print_string "x : "; print_int x; print_string "\ny : "; print_int (List.hd l_arrivee); print_newline(); *)
  let y = List.hd l_arrivee in 

    if (x = 53) then (*registre*)
        if (y = 53) then let acc = {carte = (0, Trefle); arrivee = (0, Trefle)}::acc in
              get_coups xs (List.tl l_arrivee) acc
        else 
          if (y = 52) then let acc = {carte = (0, Trefle); arrivee = (14, Trefle)}::acc in
              get_coups xs (List.tl l_arrivee) acc
          else
          let acc = {carte = (0, Trefle); arrivee = of_num(y)}::acc in
                get_coups xs (List.tl l_arrivee) acc

    else if (x = 52) then (*carte est vide*)
      if (y = 53) then let acc = {carte = (14, Trefle); arrivee = (0, Trefle)}::acc in (*arrivee registre*)
            get_coups xs (List.tl l_arrivee) acc
      else if (y = 52) then let acc = {carte = (14, Trefle); arrivee = (14, Trefle)}::acc in (*arrivee vide*)
            get_coups xs (List.tl l_arrivee) acc
      else
      let acc = {carte = (14, Trefle); arrivee = of_num(y)}::acc in (*arrivee normale*)
            get_coups xs (List.tl l_arrivee) acc

    else (*carte normale*)
      if (y = 53) then let acc = {carte = of_num(x); arrivee = (0, Trefle)}::acc in (*arrivee registre*)
            (* print_string "--------->arrivee registre\n"; *)
            get_coups xs (List.tl l_arrivee) acc
      else 
        if (y = 52) then let acc = {carte = of_num(x); arrivee = (14, Trefle)}::acc in (*arrivee vide*)
            (* print_string "--------->arrivee vide\n"; *)
            get_coups xs (List.tl l_arrivee) acc
        else
        let acc = {carte = of_num(x); arrivee = of_num(y)}::acc in (*arrivee normale*)
              get_coups xs (List.tl l_arrivee) acc
;;

(*
- convertit la liste des coups du fichier solution en et liste de carte liste d'arrivée
- utilise get_coups pour convertir au bon type de carte
- renvoie la liste de coups à jouer dans la partie
*)
let file_to_list_coups filename =
  let l = lire_fichier filename in
  (* List.map (fun x -> print_string x; print_newline()) l;; *)
  if (List.length l) = 0 then []
  else
    let l1 = List.map (fun x -> split x) l in

    let l_carte = List.map (fun x -> List.hd x) l1 in
    let l_carte = List.map (fun x -> if x="T" then 53 else (*registre c'est 53*) (*carte vide c'est 52*)
      begin 
        if x="V" then 52 
        else int_of_string x
      end
      ) l_carte 
    in
    let l_arrivee = List.map (fun x -> List.nth x 1) l1 in
    let l_arrivee = List.map (fun x -> if x="T" then 53 else 
      begin
        if x="V" then 52 
        else int_of_string x
      end ) l_arrivee in

    let liste_coup = get_coups l_carte l_arrivee [] in 

  liste_coup
;;

(*=========================================================*)
(* recherche de solutions                                  *)
(*=========================================================*)

(* partie_success : renvoie true si une partie est terminée avec toutes les cartes au dépot, false sinon*)
let partie_success partie = 
  let rec aux depot = 
    match depot with
    | [] -> true
    | x::xs -> if fst(x) = 13 then aux xs else false
  in aux partie.plateau.depot
;;

(* ajout_coup_possible_registre : renvoie la liste des coups possibles des colonnes vers les registres*)
let rec ajout_coup_possible_registre partie (acc : coup list) i = (*print_string "ajout possible de coup registre\n";*)
  if i = FArray.length partie.plateau.colonnes 
  then acc
  else if List.length (FArray.get partie.plateau.colonnes i) = 0 
    then ajout_coup_possible_registre partie acc (i+1)
    else let carte = List.hd (FArray.get partie.plateau.colonnes i) 
  in ajout_coup_possible_registre partie ({carte = carte; arrivee =  (0, Trefle)}::acc) (i+1)
;;

(* recherche_coup_registre_vers_col : renvoie la liste des coups possibles des registres vers les colonnes*)
let rec recherche_coup_registre_vers_col partie acc i = (*print_string "recherche registre\n";*)
  let rec coup_registre_aux partie acc i j = 
    if i = PArray.length partie.plateau.registre then acc
    else if j = FArray.length partie.plateau.colonnes then recherche_coup_registre_vers_col partie acc (i+1)
    else if i <> j then
      let carte = (PArray.get partie.plateau.registre i) in
      if FArray.get partie.plateau.colonnes j = [] then coup_registre_aux partie ({carte = carte; arrivee = (14, Trefle)}::acc) i (j+1) else 
      let arrivee = List.hd (FArray.get partie.plateau.colonnes j) in
      if (coup_valide partie carte arrivee) then
        coup_registre_aux partie ({carte = carte; arrivee = arrivee} :: acc) i (j+1)
      else coup_registre_aux partie acc i (j+1)
    else coup_registre_aux partie acc i (j+1)
  in coup_registre_aux partie acc i 0
;;

(* recherche_coup_colonnes : renvoie la liste des coups possibles des colonnes vers les colonnes (aka si une carte peut aller sur une autre)*)
let rec recherche_coup_colonnes partie acc i = (*print_string "recherche colonne\n";*)
  let rec coup_col_aux partie acc i j = 
    if i = FArray.length partie.plateau.colonnes then acc
    else if j = FArray.length partie.plateau.colonnes then recherche_coup_colonnes partie acc (i+1)
    else if FArray.get partie.plateau.colonnes i = [] then recherche_coup_colonnes partie acc (i+1)
    else if i <> j then
        let carte = List.hd (FArray.get partie.plateau.colonnes i) in
        if FArray.get partie.plateau.colonnes j = []
          then coup_col_aux partie ({carte = carte; arrivee = (14, Trefle)}::acc) i (j+1)
          else 
        let arrivee = List.hd (FArray.get partie.plateau.colonnes j) in
        if (coup_valide partie carte arrivee) then
        coup_col_aux partie ({carte = carte; arrivee = arrivee}::acc) i (j+1)
      else coup_col_aux partie acc i (j+1)
    else coup_col_aux partie acc i (j+1)
    in coup_col_aux partie acc i 0;;
   

(*recherche_coup_possibles_registre : verifie si une carte peut aller au registre (aka si il existe un registre vide, si oui, ajoute ce coup à la liste de coup)*)
let recherche_coup_possibles_registre (partie : partie) acc =
  if registre_vide partie.plateau.registre 
    then ajout_coup_possible_registre partie acc 0
    else acc
;;

(* recherche_coup_registre_colonnes : renvoie la liste des coups possibles des registres vers les colonnes*)
let recherche_coup_registre_colonnes partie acc = 
  if PArray.exists (fun x -> x <> (0, Trefle)) partie.plateau.registre 
    then recherche_coup_registre_vers_col partie acc 0
    else [];;


let best_score_coup liste_coup partie = 
  let rec aux liste_coup partie best_coup best_score = 
    match liste_coup with
    | [] -> best_coup
    | x::xs -> 
      let tmp_partie = add_coup partie x in
      if tmp_partie.plateau.score > best_score then aux xs partie x partie.plateau.score
      else aux xs partie best_coup best_score
  in
  aux liste_coup partie (List.hd liste_coup) 0
;;

let remove_coup_liste_coup liste_coup coup = 
  let rec aux liste_coup acc = 
    match liste_coup with
    | [] -> acc
    | x::xs -> if x = coup then aux xs acc else aux xs (x::acc)
  in aux liste_coup []


(*renvoie la colonne d'une carte*)
let colonne_carte carte partie = 
  let rec aux i = 
    if i = FArray.length partie.plateau.colonnes then -1
    else if List.mem carte (FArray.get partie.plateau.colonnes i) then i
    else aux (i+1)
  in aux 0
;;

(*optimisation 1 : si carte vient d'une colonne de 1 elem vers colonne vide*)
let carte_seule_to_vide coup partie = 
  if ((fst(coup.arrivee)) = 14) && (List.length (FArray.get partie.plateau.colonnes (colonne_carte coup.carte partie)) = 1) then true
  else false
;;

(*optimisation 2 : si on a plusieurs colonnes vides, pas besoin de considérer le déplacement d'une carte vers toutes ces colonnes vides, le faire vers la première suffira.*)
(*on supprime donc les doublons dans la liste*)
let rec remove_doublons liste_coup = 
  match liste_coup with
  | [] -> []
  | x::xs -> if List.mem x xs then remove_doublons xs else x::(remove_doublons xs)
;;

(*optimisation 3 : Comme en plus ces parties ont un usage contraint des colonnes vides, alors une longue séquence de cartes décroissantes de même couleur ne pourra plus être déplacée, et ne pourra évoluer que via une mise au dépôt. Par "longue", on entend ici (n+2) cartes au moins si l'on a n registres. Dans cette même colonne, si enfin une "petite" carte de la même couleur se trouve bloquée quelque part sous la "longue" séquence, alors la mise au dépôt de ces cartes sera toujours impossible. On pourra donc chercher en sommet de colonnes de telles "longues séquences bloquées", et supprimer de la recherche les états qui en contiennent, car ils sont insolubles.*)
let longue_sequence_bloquee_mo colonne partie = 
  if partie.config.game = Midnight then
    if List.length colonne < 3 then false
    else
      let rec aux colonne = 
        match colonne with
        | [] -> false
        | x::[] -> false
        | x1::x2::[] -> false
        | x1::x2::xs -> if (not(is_opposite_color x1 x2)) && (not(is_opposite_color x2 (List.hd xs))) && (fst x1 > fst x2) && (fst x2 > fst (List.hd xs)) then true else aux (x2::xs)
        in aux colonne
  else false
;;


let longue_sequence_bloquee_st colonne partie = 
  if partie.config.game = Seahaven then
    if List.length colonne < 6 then false
    else
      let rec aux colonne = 
        match colonne with
        | [] -> false
        | x::[] -> false
        | x1::x2::[] -> false
        | x1::x2::x3::[] -> false
        | x1::x2::x3::x4::[] -> false
        | x1::x2::x3::x4::x5::[] -> false
        | x1::x2::x3::x4::x5::xs -> if (not(is_opposite_color x1 x2)) && (not(is_opposite_color x2 x3)) && (not(is_opposite_color x3 x4)) && (not(is_opposite_color x4 x5)) && (not(is_opposite_color x5 (List.hd xs))) && (fst x1 > (fst x2) ) && (fst x2 > (fst x3) ) && (fst x3 > (fst x4) ) && (fst x4 > (fst x5) ) && (fst x5 > (fst (List.hd xs)))
            then true
          else aux xs
      in aux colonne
  else false
;;

let existe_longue_sequence_bloquee partie = 
  print_string "existe_longue_sequence_bloquee\n";
  let rec aux i = 
    if i = FArray.length partie.plateau.colonnes then false
    else if longue_sequence_bloquee_mo (FArray.get partie.plateau.colonnes i) partie then true
    else if longue_sequence_bloquee_st (FArray.get partie.plateau.colonnes i) partie then true
    else aux (i+1) 
  in aux 0
;;

(*supprime les coups qui ne sont pas optimisés*)
let optimisation_list liste_coup partie = 
  let rec aux liste_coup partie acc = 
    match liste_coup with
    | [] -> remove_doublons acc
    | x::xs -> 
      begin
        (*si la carte provient d'une liste vide vers une colonne vide*)
        (*supprimer le coup*)
        if (carte_seule_to_vide x partie) || (coup_valide partie x.carte x.arrivee == false)
          then
            begin
              (* print_string "coup refusé : "; *)
              (* coup_to_string x; *)
            (* print_int (colonne_carte x.carte partie); print_string "--->colonne de 1 elem\n"; *)
            (* if (longue_sequence_bloquee_mo (FArray.get partie.plateau.colonnes (colonne_carte x.carte partie)) partie) then print_string "longue sequence bloquee : "; *)
            aux xs partie acc
            end
        else
          aux xs partie (x::acc)
      end
  in aux liste_coup partie []
;;

(* recherche_coup_possibles : renvoie tous les coups possibles d'une partie*)
let recherche_coup_possibles partie =  (recherche_coup_possibles_registre partie (recherche_coup_colonnes  partie (recherche_coup_registre_colonnes partie []) 0)) 
;;


let list_coup_optimise partie = 
  let liste_coup = recherche_coup_possibles partie in
  let liste_coup = optimisation_list liste_coup partie in
  remove_doublons liste_coup
;;


let print_liste_coups_possibles partie =
  print_string "\nCoups possibles : \n"; List.iter (fun x -> coup_to_string x) (recherche_coup_possibles partie)
;;
let print_liste_coups_opt list =
  print_string "\nCoups possibles optimises : \n"; List.iter (fun x -> coup_to_string x) list
;;

(*chercher_sol : *)
let rec chercher_sol partie filename partie_init = 
  if (partie_success (mise_au_depot partie)) then 
    begin
      list_coup_to_file filename partie.plateau.liste_coup;
      print_string "SUCCES\n";
      exit 0;
    end
  else

    print_string "\ncompteur de coups de la partie: "; print_int partie.plateau.compteur_coup; print_newline();

    let partie = mise_au_depot partie in

    let liste_coup = list_coup_optimise partie in

    let rec aux liste_coup partie max_score = (*print_partie partie;*)
      print_string "\nscore: "; 
      print_int partie.plateau.score; 
      print_newline(); 
      print_string "\nnb coups possibles:";
      print_int (List.length liste_coup); 
      print_newline();

      (* print_liste_coups_possibles partie; print_newline();*)
      print_liste_coups_opt liste_coup; print_newline();
      let max_score = if partie.plateau.score > max_score then partie.plateau.score else max_score in
      if liste_coup = [] then
        begin
          if (partie_success (mise_au_depot partie)) then 
            begin
              list_coup_to_file filename partie.plateau.liste_coup;
              print_string "SUCCES\n";
              exit 0;
            end
          else
            if (partie.plateau.compteur_coup = 0) then
              begin
                print_string "INSOLUBLE\n";
                exit 2;
              end
              (*principe du parcours en profondeur*)
              (*si la branche est insoluble*)
              (*on revient en arriere*)
              (*on recherche sur la partie précédente d'autre coups possibles*)
              (*autre que celui qui a mené à la branche insoluble*)
              (*on enleve le dernier coup de la liste de coup*)
              (*on enleve le dernier plateau de l'historique*)
              (*on recommence la recherche de coup possible*)
        end
      else
        begin
          let best_coup = best_score_coup liste_coup partie in
          let tmp_partie = mise_au_depot (add_coup (partie) best_coup) in
          if (Histo_plateau.mem tmp_partie.plateau partie.histo_plateau)
            then 
              begin
                let test = compare_parties partie_init.plateau tmp_partie.plateau in
                print_string "\ntest comparaison: ";
                print_int test;
                print_string "\nSi on joue ce coup on revient sur un plateau déjà vu :";
                coup_to_string best_coup; 
                print_newline();
                let liste_coup = remove_coup_liste_coup liste_coup best_coup in
                aux liste_coup partie max_score
              end
          else 
          if (existe_longue_sequence_bloquee tmp_partie) || (tmp_partie.plateau.score < max_score - 10) then
            begin
              print_string "\nSi on joue ce coup on a une longue sequence bloquee ou score trop bas :";
              coup_to_string best_coup;
              print_newline();
              let partie = {partie with histo_plateau = Histo_plateau.add partie.plateau tmp_partie.histo_plateau} in
              let liste_coup = remove_coup_liste_coup liste_coup best_coup in
              aux liste_coup partie max_score
            end
          else 
            begin
              print_string "\ncoup à jouer : "; 
              coup_to_string best_coup;
              print_newline();
              (* print_partie tmp_partie;*)
              let liste_coup = remove_coup_liste_coup liste_coup best_coup in
              chercher_sol tmp_partie filename partie_init;
              print_string "On revient en arrière\n";
              aux liste_coup partie max_score (* else *);
            end
        end
    in aux liste_coup partie 0
;;

(*=========================================================*)
    
let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")

(* TODO : La fonction suivante est à adapter et continuer *)

let rec print_c_c_list (l : card list list) =
  match l with
  | [] -> print_newline()
  | hd::tl -> List.iter (fun x -> print_string (to_string x); print_string " ") hd; print_newline(); print_c_c_list tl
;;

(* Pourra etre enlevé... *)
let file_name conf = 
  match conf.mode with
  | Check filename -> filename
  | Search filename -> filename
;;

let rec print_list_coup liste_coup= 
  match liste_coup with
  | [] -> print_string "."
  | x :: xs -> coup_to_string x; print_list_coup xs
;;

let faire_mod config permut =
  match config.mode with
  | Check filename -> let fin = jouer_partie (mise_au_depot((init_partie config.game config.seed config.mode (List.map (Card.of_num) permut)))) (file_to_list_coups filename) 1 in print_newline()
  | Search filename -> let fin = chercher_sol (init_partie config.game config.seed config.mode (List.map (Card.of_num) permut)) 
                                  (filename)
                                  (init_partie config.game config.seed config.mode (List.map (Card.of_num) permut)) 
                                in print_newline()
    
  let print_mode conf =
  match conf.mode with
  | Check filename -> print_string "Check "
  | Search filename -> print_string "Search "

let treat_game conf =
  (* print_liste_coups (file_to_list_coups "test.sol"); *)
  print_string "Jeu : ";
  print_string (game_to_string conf.game);
  print_newline ();
  print_string "Graine : ";
  print_int conf.seed;
  print_newline ();
  print_string "Fichier : ";
  print_string (file_name conf);
  print_newline ();
  print_string "Mode :";
  print_mode conf;
  let permut = XpatRandom.shuffle conf.seed in
  Printf.printf "Permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;
  print_newline ();
  faire_mod conf permut;
  print_partie (init_partie conf.game conf.seed conf.mode (List.map (Card.of_num) permut));
  (*let fin = jouer_partie(init_partie conf.game conf.seed conf.mode (List.map (Card.of_num) permut)) (file_to_list_coups (file_name conf)) 1 in
  print_newline ()*);;

let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config

let _ = if not !Sys.interactive then main () else ();;