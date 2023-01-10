open Types
open XpatLib
open Card
open FArray

(* initialise une FArray selon la partie en cours *)

let array_init game = let n = 
  match game with
  | Freecell -> 8
  | Midnight -> 18
  | Seahaven -> 10
  | Baker-> 13
  in FArray.make n [];;
;;