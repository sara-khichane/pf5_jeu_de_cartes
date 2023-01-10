open Types
open XpatLib
open Card
open FArray
open PArray
open Registres
open Colonnes


let longueur_colonnes game = match game with
| Freecell -> 7
| Seahaven -> 5
| Midnight -> 3
| Baker -> 4
;;