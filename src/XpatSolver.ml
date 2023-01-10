open XpatLib
open Card
open PArray
open FArray
open Types
open Colonnes
open Registres
open Depot
open Initialisation
open Coup
open Fichier

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


  

(*=========================================================*)
(* affichage d'une partie                                  *)
(*=========================================================*)

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
  let partie = mise_au_depot partie in
  let rec aux liste_coup partie best_coup best_score = 
    match liste_coup with
    | [] -> best_coup
    | x::xs -> 
      let tmp_partie = add_coup partie x in
      if tmp_partie.plateau.score > best_score then aux xs partie x tmp_partie.plateau.score
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
  (* print_string "existe_longue_sequence_bloquee\n"; *)
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
      (* print_string "\nscore: "; 
      print_int partie.plateau.score; 
      print_newline(); 
      print_string "\nnb coups possibles:";
      print_int (List.length liste_coup); 
      print_newline(); *)

      (* print_liste_coups_possibles partie; print_newline();*)
      (* print_liste_coups liste_coup; print_newline(); *)
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
                (* print_string "\ntest comparaison: ";
                print_int test;d
                print_string "\nSi on joue ce coup on revient sur un plateau déjà vu :";
                coup_to_string best_coup; 
                print_newline(); *)
                let liste_coup = remove_coup_liste_coup liste_coup best_coup in
                aux liste_coup partie max_score
              end
          else 
          if (existe_longue_sequence_bloquee tmp_partie) || (tmp_partie.plateau.score < max_score - 10) then
            begin
              (* print_string "\nSi on joue ce coup on a une longue sequence bloquee ou score trop bas :";
              coup_to_string best_coup;
              print_newline(); *)
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
              (* print_string "On revient en arrière\n"; *)
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