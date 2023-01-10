open Types
open XpatLib
open Card
open PArray

val init_registres : game -> card list -> card PArray.t

val registre_vide : card PArray.t -> bool

val ajout_registres : card t -> card -> card t

val enlever_ifexists_carte_registre : (cardnum * suit) t -> cardnum * suit -> (cardnum * suit) t