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

type plateau = { colonnes: card list FArray.t ; registre : card PArray.t ; depot : card list };;
type partie = {mutable config : config; mutable plateau : plateau (*;mutable liste_coup : coup list; mutable compteur : int *) };;
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
(*
type list FArray = {
  mutable contents : list array;
  default : [];
  mutable nb : int;
}*)

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
- FreeCell 4 registres temporaires, Initialement, les registres sont vides, un registre vide peut recevoir une carte.
- Seahaven 4 registres temporaires, Initialement, les deux dernières cartes de la liste des colonnes sont dans les registres, un registre vide peut recevoir une carte.
- Aucun registre pour Midnight Oil et Baker's Dozen.
- Les registres sont des PArray de carte.
*)

(*carte par défaut (0, Trefle)*)
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

(* retourne true s'il existe un registre vide, false sinon on utilise*)
let registre_vide registres =
  let rec aux i =
    if i = PArray.length registres then false
    else if PArray.get registres i = (0, Trefle) then true
    else aux (i+1)
  in
  aux 0
;;

(* ajoute une carte dans les registres et trier le parray de registres de tel sorte que les vides soient en premiers*)
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


(* enleve la première carte trouvée des registres //si besoin plus tard *)
let enleve_registre registres =
  let rec aux i =
    if PArray.get registres i <> None then
      PArray.set registres i None
    else
      aux (i+1)
  in
  aux 0
;;

(*si la carte se trouve dans les registres, on l'enlève*)
let enlever_ifexists_carte_registre registres carte =
  PArray.map (fun x -> if (x = carte) then (0, Trefle) else x) registres
;;



(*=========================================================*)
(* Structure de gestion du depot                           *)
(*=========================================================*)

(* initialisation de depot *)
(*initialiser le depot avec des cartes trefle*)
(*REVOIR AJOUT CARTE, COMMENT RECUP SI UNE CARTE A ETE MISE AU DEPOT*)

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
    FArray.map (fun x -> if x = [] then [] else if (List.hd x = arrivee) then carte::x else x) colonnes;;
  
(*SI REMPLISSAGE DUNE CASE VIDE, ICI *)
let exists_colonne_vide colonnes = FArray.exists (fun x -> x = []) colonnes;;
let carte_seule_dans_colonne colonnes carte = FArray.exists (fun x -> if x = [] then false else List.hd x = carte && List.tl x = []) colonnes;; (*HDLIST*)
(* ajout d'une carte au depot *)
(* enlève une carte des colonnes / registres *)
(* return une partie *)
let ajout_carte_depot partie (carte : Card.card) = 
  print_string "---ajout carte depot----";
  let depot = List.map (fun x -> if ((snd(x)) = (snd(carte))) then (fst(x)+1, snd(x)) else x) (partie.plateau.depot)  in
  let colonnes = FArray.map (fun x -> if (x <> [] && (List.hd x = carte)) then List.tl x else x) partie.plateau.colonnes in (*HDLIST*)
  let registre = enlever_ifexists_carte_registre partie.plateau.registre carte in
  let plateau = { colonnes = colonnes; registre = registre; depot = depot} in
{config = partie.config ; plateau = plateau; };; (*partie.liste coup et partie.compteur pour jalon 2*)

(* si une carte peut être mise au depot alors on enlève cette carte du plateau et on la rajoute au depot *)
(*let carte_to_depot2 partie carte = 
  let rec carte_to_depot_aux partie carte acc = match acc with
  | [] -> partie
  | hd::tl when (snd(hd)) = (snd(carte)) && fst(hd) = (fst(carte) - 1) -> (ajout_carte_depot partie carte)
  | hd::tl -> carte_to_depot_aux partie carte tl
in carte_to_depot_aux partie carte partie.plateau.depot;;*)

let carte_to_depot partie carte = 
  let rec carte_to_depot_aux partie carte acc = match acc with
  | [] -> false
  | hd::tl when (snd(hd)) = (snd(carte)) && fst(hd) = (fst(carte) - 1) ->  print_newline(); print_string (Card.to_string(carte)); print_string "->la carte doit etre mise au depot\n"; true
  | hd::tl -> carte_to_depot_aux partie carte tl
in carte_to_depot_aux partie carte partie.plateau.depot;;

(* fonction : si la carte en tête de la colonne peut etre mise au depot, alors on la met et on rappelle la fonction sur la carte d'après *)
(* let rec fonction_mise_au_depot partie colonne = if ((carte_to_depot partie (List.hd colonne)) = partie) then colonne else fonction_mise_au_depot partie (List.tl colonne);; (*None enlevé, partie à sa place --> à revoir*) *)

(* applique fonction_mise_au_depot à toutes les colonnes *)
(* let mise_au_depot config partie = FArray.iter fonction_mise_au_depot (partie.plateau.colonnes) ;; (*a revoir*) *)

(* fonction : si la carte en tête de la colonne peut etre mise au depot, alors on la met et on rappelle la fonction sur la carte d'après *)
(*
let rec fonction_mise_au_depot partie colonne = if ((carte_to_depot partie (List.hd colonne)) = partie) then partie else fonction_mise_au_depot partie (List.tl colonne);;
*)
(* applique fonction_mise_au_depot à toutes les colonnes *)
(*let mise_au_depot2 partie = 
  let partie = {partie with plateau = 
    {partie.plateau with colonnes = FArray.map (fun x -> fonction_mise_au_depot partie x) (partie.plateau.colonnes)}} in
    partie
;; (*a revoir*)
*)
(* 
let mise_au_depot_registre2 partie = 
  let registres = PArray.map (fun x ->if (carte_to_depot partie x) then (0, Trefle) else x) partie.plateau.registre in 
  let plateau = { colonnes = partie.plateau.colonnes; registre = registres; depot = partie.plateau.depot} in
   {config = partie.config ; plateau = plateau; };; *)

(*ces deux fonctions dépendent s'appellent l'une à l'autre*)
let rec mise_au_depot_registre (partie : partie)= 
  let rec mise_au_depot_registre_aux partie acc = 
    if acc = PArray.length partie.plateau.registre then partie
    else
    if (carte_to_depot partie (PArray.get partie.plateau.registre acc)) then 
    begin 
      print_string "\ndu registre au depot ---> ";
      print_string (Card.to_string (PArray.get partie.plateau.registre acc)); 
      let new_partie = mise_au_depot_registre_aux (ajout_carte_depot partie (PArray.get partie.plateau.registre acc)) 0
    in mise_au_depot new_partie
  end
    else mise_au_depot_registre_aux partie (acc+1)
  in mise_au_depot_registre_aux partie 0
  
and mise_au_depot partie = 
  let rec mise_au_depot_aux partie acc = 
    if acc = FArray.length partie.plateau.colonnes then begin print_string "\nfin verif colonnes pour depot\n"; (mise_au_depot_registre partie) end
    else
      if (FArray.get partie.plateau.colonnes acc) = [] then begin print_newline(); print_int acc; mise_au_depot_aux partie (acc+1) end
      else
      begin
        print_string "- la premiere de la colonne : ";
        print_string (Card.to_string (List.hd (FArray.get partie.plateau.colonnes acc)));
        if (carte_to_depot partie (List.hd (FArray.get partie.plateau.colonnes acc))) then begin  print_newline(); print_int acc; print_string "- des colonnes au depot ---> "; print_string (Card.to_string (List.hd (FArray.get partie.plateau.colonnes acc))); mise_au_depot_aux (ajout_carte_depot partie (List.hd (FArray.get partie.plateau.colonnes acc)) ) 0 end
        else begin print_newline(); print_int acc; mise_au_depot_aux partie (acc+1) end
      end
  in
    mise_au_depot_aux partie 0;;


(*=========================================================*)
(* Init une partie                                         *)
(*=========================================================*)

let longueur_colonnes game = match game with
| Freecell -> 7
| Seahaven -> 5
| Midnight -> 3
| Baker -> 4
;;

(* VERIFIER FONCTIONNEMENT *)
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
(* CA MARCHE MAIS JSUIS PAS SURE*)
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
registre = init_registres config.game liste_permut; depot = depot_init}
;; 
(* let plateau_init config liste_permut = {colonnes = remplir_colonne (list_to_split_list liste_permut config.game); 
registre = init_registres config.game liste_permut; depot = depot_init}
;;  *)

(*=========================================================*)
(* AFFICHAGE                                               *)
(*=========================================================*)
let print_partie partie = 
  (* print_string "\nles testes d'helo\n";
  print_string "farray de la colonne 0 : \n";
  List.iter (fun x -> print_string (Card.to_string x); print_string" ") (List.rev (FArray.get partie.plateau.colonnes (0)));
  print_string "\ntête de la colonne 0 : "; *)
  (*print_string (Card.to_string (List.hd (FArray.get partie.plateau.colonnes (0))));*)
  print_string "\n\nSens de lecture des colonnes : -> \n";
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
;;


(*=========================================================*)
(* GESTION DES COUPS                                       *)
(*=========================================================*)

type coup = { 
  carte : card; 
  arrivee : card;
}
let coup_to_string (coup : coup)= 
  print_string "[ ";
  print_string (to_string (coup.carte));
  print_string " ; ";
  print_string (to_string (coup.arrivee));
  print_string " ]\n";;


type histo_coup = coup list;;

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
            print_string "\nif exists registre vide : ";
            print_bool (registre_vide partie.plateau.registre);
            registre_vide partie.plateau.registre
          end
        | Seahaven -> registre_vide partie.plateau.registre
        | Midnight -> false
        | Baker -> false
      else
        match partie.config.game with
        | Freecell -> if (is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else 
          begin
            print_string "\nis_opposite_color / bon_nombre : "; print_bool (is_opposite_color carte arrivee) ; print_string " "; print_bool (bonnombre carte arrivee); print_string "\n"; false;
          end
        | Seahaven -> if not(is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else false
        | Midnight -> if (is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else false
        | Baker -> if (bonnombre carte arrivee) then true else false
  ;;

(* let trouver_coup = failwith "TODO";; (*partie 2*) *)

(* let add_coup_history coup party = coup :: party.liste_coup;;  (*partie 2*)*)
 
let add_coup partie coup =
  (* Printf.printf "Fonction add_coup : Coup : "; (coup_to_string coup) ; *)
  if coup_valide partie coup.carte coup.arrivee then (*rajouter les fonctions ajouter et enlever*)
    if fst(coup.arrivee) = 0 then
      let partie = {partie with plateau = {colonnes = retirer_carte_colonnes partie.plateau.colonnes coup.carte; registre = ajout_registres partie.plateau.registre coup.carte; depot = partie.plateau.depot}} in
      partie
    else
      let partie = {partie with plateau = {colonnes = ajouter_carte_colonnes (retirer_carte_colonnes partie.plateau.colonnes coup.carte) coup.carte coup.arrivee; depot = partie.plateau.depot; registre = (enlever_ifexists_carte_registre partie.plateau.registre coup.carte)}} in
      partie
  else
    partie
;;

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
        print_partie partie;
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
      print_string "\nbefore maj depot\n";
      let partie = mise_au_depot (add_coup partie x) in
      print_string "\nafter maj depot\n";
      jouer_partie partie xs (i+1)

;;

(*=========================================================*)
(* LECTURE DU FICHIER                                      *)
(*=========================================================*)

  let lire_fichier filename = 
    let rec aux filename acc = 
      try 
        let x = input_line filename in 
        aux filename (x::acc)
      with End_of_file -> acc
    in List.rev (aux (open_in filename) []);;

    let split x =
      String.split_on_char ' ' x
    ;;

    (*recup les listes des carte et des arrivee des coup*)

  (*creer la liste des coup*)
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

  let file_to_list_coups filename =

    let l = lire_fichier filename in
    (* List.map (fun x -> print_string x; print_newline()) l;; *)
    if (List.length l) = 0 then []
    else
      let l1 = List.map (fun x -> split x) l in

      let l_carte = List.map (fun x -> List.hd x) l1 in
      let l_carte = List.map (fun x -> if x="T" then 53 else 
        begin 
          if x="V" then 52 
          else int_of_string x
        end
        ) l_carte in (*registre est 53*) (*carte vide c'est 52*)

      let l_arrivee = List.map (fun x -> List.nth x 1) l1 in

      (* print_string "registre  : "; (*a enlever*)
      print_string (List.hd l_arrivee);
      print_newline(); *)

      let l_arrivee = List.map (fun x -> if x="T" then 53 else 
        begin
          if x="V" then 52 
          else int_of_string x
        end ) l_arrivee in

      (* print_string "registre after num : "; (*a enlever*)
      print_int (List.hd l_arrivee);
      print_newline(); *)

      let liste_coup = get_coups l_carte l_arrivee [] in 

    liste_coup
  ;;

(*=========================================================*)

let init_partie game seed mode liste_permut = let config = {game = game; seed = seed; mode =  mode } in 
  {config = config; plateau = (plateau_init config liste_permut) (*;liste_coup = partie.liste_coup; compteur = partie.compteur*)};;
    
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
let file_name  conf= match conf.mode with
| Check filename -> filename
| Search filename -> filename
;; 
let rec print_list_coup liste_coup= 
match liste_coup with
| [] -> print_string "."
| x :: xs -> coup_to_string x; print_list_coup xs;;
let treat_game conf =
  print_string "Jeu : ";
  print_string (game_to_string conf.game);
  print_newline ();
  print_string "Graine : ";
  print_int conf.seed;
  print_newline ();
  print_string "Fichier : ";
  print_string (file_name conf);
  print_newline ();
  let permut = XpatRandom.shuffle conf.seed in
  Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;
  print_newline ();
  (* testes *)
  (*print_string "\nListe permut : "; PERMET DE PRINT LA LISTE DE PERMUTATION SCINDEE
  print_c_c_list(list_to_split_list (List.map (Card.of_num) permut) conf.game);*) 
  (*List.iter (fun x -> print_string (to_string x); print_string " ") (List.map (Card.of_num) permut);*)
  (*  *)
 
  print_partie (init_partie conf.game conf.seed conf.mode (List.map (Card.of_num) permut));
  (*print_partie (add_coup (init_partie conf.game conf.seed conf.mode (List.map (Card.of_num) permut)) {carte = (2, Carreau); arrivee = ( 0, Trefle)});*)
  (*print_partie(mise_au_depot(init_partie conf.game conf.seed conf.mode (List.map (Card.of_num) permut)));*)
  let fin = jouer_partie(init_partie conf.game conf.seed conf.mode (List.map (Card.of_num) permut)) (file_to_list_coups (file_name conf)) 1 in
  print_partie fin;;


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