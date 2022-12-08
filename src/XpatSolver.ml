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
      let registres = PArray.set registres 2 (List.nth permut (List.length permut)) in
      let registres = PArray.set registres 3 (List.nth permut (List.length permut - 1)) in
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

(* ajout d'une carte au depot *)
(* enlève une carte des colonnes / registres *)
(* return une partie *)
let ajout_carte_depot partie (carte : Card.card) = 
  let depot = List.map (fun x -> if ((snd(x)) = (snd(carte))) then (fst(x)+1, snd(x)) else x) (partie.plateau.depot)  in
  let colonnes = FArray.map (fun x -> if (List.hd x = carte) then List.tl x else x) partie.plateau.colonnes in
  let registre = enlever_ifexists_carte_registre partie.plateau.registre carte in
  let plateau = { colonnes = colonnes; registre = registre; depot = depot} in
{config = partie.config ; plateau = plateau; };; (*partie.liste coup et partie.compteur pour jalon 2*)

(* si une carte peut être mise au depot alors on enlève cette carte du plateau et on la rajoute au depot *)
let carte_to_depot partie carte = 
  let rec carte_to_depot_aux partie carte acc = match acc with
  | [] -> partie
  | hd::tl when (snd(hd)) = (snd(carte)) && fst(hd) = (fst(carte) - 1) -> (ajout_carte_depot partie carte)
  | hd::tl -> carte_to_depot_aux partie carte tl
in carte_to_depot_aux partie carte partie.plateau.depot;;


(* fonction : si la carte en tête de la colonne peut etre mise au depot, alors on la met et on rappelle la fonction sur la carte d'après *)
(* let rec fonction_mise_au_depot partie colonne = if ((carte_to_depot partie (List.hd colonne)) = partie) then colonne else fonction_mise_au_depot partie (List.tl colonne);; (*None enlevé, partie à sa place --> à revoir*) *)

(* applique fonction_mise_au_depot à toutes les colonnes *)
(* let mise_au_depot config partie = FArray.iter fonction_mise_au_depot (partie.plateau.colonnes) ;; (*a revoir*) *)

(* fonction : si la carte en tête de la colonne peut etre mise au depot, alors on la met et on rappelle la fonction sur la carte d'après *)
let rec fonction_mise_au_depot partie colonne = if ((carte_to_depot partie (List.hd colonne)) = partie) then colonne else fonction_mise_au_depot partie (List.tl colonne);;

(* applique fonction_mise_au_depot à toutes les colonnes *)
let mise_au_depot partie = FArray.map (fun x -> fonction_mise_au_depot partie x) (partie.plateau.colonnes) ;; (*a revoir*)


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
    if list = [] then liste_finale 
    else match compteur_carte with
    (* si rangée impaire, alors compteur colonne mod 2 = 1, sachant que taille doit etre egale à 7, 6+1 = 7*)
    | x when x = (taille_colonne + (compteur_colonne mod 2)- 1) -> aux list [] taille_colonne (compteur_colonne + 1) 0 (liste_finale @ [List.rev acc])
    | x -> aux (List.tl list) ((List.hd list)::acc) taille_colonne compteur_colonne (compteur_carte + 1) liste_finale
	in aux list [] 7 0 0 [[]];;

(* peut etre optimisé pour enlever le @ List.rev ?*)
(*problème pour seahven et ses registres ? -> liste_permut pas vide *)
let list_to_split_list (list : card list ) game =
  let rec aux list acc taille_colonne compteur_colonne compteur_carte liste_finale = 
    if list = [] then if acc = [] then liste_finale else (liste_finale @ [List.rev acc]) 
    else match compteur_carte with
    | x when x = (taille_colonne - 1) -> aux list [] taille_colonne (compteur_colonne + 1) 0 (liste_finale @ [List.rev acc])
    | x -> aux (List.tl list) ((List.hd list)::acc) taille_colonne compteur_colonne (compteur_carte + 1) liste_finale
  in if game = Freecell then list_to_split_list_freecell list game
  else aux list [] (longueur_colonnes game) 0 0 [[]];;
;;
(* CA MARCHE MAIS JSUIS PAS SURE*)
(*remplie les colonnes avec les listes de cartes dans la liste l*)
let rec remplir_colonne ( list: card list list) colonnes n =
 match n with
  | n when n = (length colonnes - 1) -> colonnes
  | n -> let farray = FArray.set colonnes n (List.hd list) in remplir_colonne (List.tl list) colonnes (n+1);; (*AVANT : INSTRUCTION 1 ; 2*)
  (*SOLUTION A VOIR : let remplir_colonne2 = of_list l;; *)

(* FREECELL PAS ENCORE FONCTIONNEL *)
let plateau_init config liste_permut = {colonnes = remplir_colonne (list_to_split_list liste_permut config.game) (array_init config.game) ((* FArray.length partie.plateau.colonnes*) longueur_colonnes config.game); 
                                      registre = init_registres config.game liste_permut; depot = depot_init}
;;
    
(*=========================================================*)
(* AFFICHAGE                                               *)
(*=========================================================*)
let print_partie partie = 
  print_string "\n\n";
  for i = 0 to FArray.length partie.plateau.colonnes - 1 do
    print_string "Colonne : \n ";
    List.iter (fun x -> print_string (Card.to_string x)) (FArray.get partie.plateau.colonnes (i));
    print_newline();
  done;
  print_string "\nRegistre : ";
  PArray.iter (fun x -> print_string (Card.to_string x); print_string " " ) partie.plateau.registre;

print_string "\n\nDepot : ";
List.iter (fun x -> print_string (Card.to_string x); print_string " " ) partie.plateau.depot;
print_newline();;


(*=========================================================*)
(* GESTION DES COUPS                                       *)
(*=========================================================*)

type coup = { 
  carte : card; 
  colonne_arriv : card list;
}

type histo_coup = coup list;;

let is_opposite_color card1 card2 = 
	match snd(card1) with
	| Trefle | Pique when snd(card2) =  Coeur || snd(card2) = Carreau -> true
	| Coeur | Carreau when  snd(card2) =  Trefle || snd(card2) = Pique -> true
	| _ -> false ;;

let bonnombre carte arrivee =
    if fst(carte) = (fst(arrivee) + 1) then true 
    else false

(*Fonction qui check si c'est possible de placer la carte carte sur arrivee*)
let coup_valide config carte colonne_arriv = 
  let arrivee = List.hd colonne_arriv in
    if colonne_arriv = [] then
      match config.game with
      | Freecell -> true
      | Seahaven -> if fst(carte) = 13 then true else false
      | Midnight -> false
      | Baker-> false
    else
      match config.game with
      | Freecell -> if (is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else false
      | Seahaven -> if not(is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else false
      | Midnight -> if (is_opposite_color carte arrivee) && (bonnombre carte arrivee) then true else false
      | Baker -> if (bonnombre carte arrivee) then true else false
  ;;


(* let trouver_coup = failwith "TODO";; (*partie 2*) *)

(* let add_coup_history coup party = coup :: party.liste_coup;;      JALON 2*)

              
let add_coup partie coup =
  if coup_valide partie.config coup.carte coup.colonne_arriv then
    let partie = ajout_carte_depot partie coup.carte in
    partie
  else
    partie
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

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;
  print_newline ();
  (*testes *)
  print_string "\nListe permut : ";
  List.iter (fun x -> print_string (to_string x); print_string " ") (List.map (Card.of_num) permut);
  (*  *)
 
  print_partie (init_partie conf.game conf.seed conf.mode (List.map (Card.of_num) permut));
  exit 0

let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config

let _ = if not !Sys.interactive then main () else ()
