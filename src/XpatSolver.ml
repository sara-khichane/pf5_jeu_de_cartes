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
open Recherche
open Optimisation
open Affichage

(*=========================================================*)
(* Validation de Solution -Rendu 1-                        *)
(*=========================================================*)

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
(* Recherche de Solution -Rendu 2-                         *)
(*=========================================================*)

(* partie_success : 
  renvoie true si une partie est terminée avec toutes les cartes au dépot, false sinon*)
  let partie_success partie = 
    let rec aux depot = 
      match depot with
      | [] -> true
      | x::xs -> if fst(x) = 13 then aux xs else false
    in aux partie.plateau.depot
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
    
let config = { game = Freecell; seed = 1; mode = Search "" }

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

let rec print_c_c_list (l : card list list) =
  match l with
  | [] -> print_newline()
  | hd::tl -> List.iter (fun x -> print_string (to_string x); print_string " ") hd; print_newline(); print_c_c_list tl
;;

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
  print_partie (init_partie conf.game conf.seed conf.mode (List.map (Card.of_num) permut));;

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