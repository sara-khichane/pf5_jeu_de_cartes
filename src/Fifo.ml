(** A FIFO structure (First-In First-Out),
    implemented in functional style
    (NB: the Queue module of OCaml stdlib is imperative)

    NB: l'implémentation fournie initialement ci-dessous est inefficace,
    l'améliorer (tout en restant fonctionnel). Par exemple on peut utiliser
    une paire de listes pour implémenter ['a t].

*)

let append_rev left right =
  let rec aux acc l =
    match l with
    | [] -> acc
    | x::r -> aux (x::acc) r
  in aux right left ;;

(* first list is the front of the queue, second list is the back of the queue *)
type 'a t = 'a list * 'a list
let empty = ([], [])
let push x q = 
  match q with
  | (front, back) -> (front, x :: back)
let pop q = 
  match q with
  | (front, back) -> 
    match front with
    | [] -> (match List.rev back with
        | [] -> raise Not_found (*si les deux listes sont vides, la fifo est vide, on renvoie une erreur*)
        | x :: reste -> (x, (reste, []))) (*si la première liste est vide, on reverse la deuxieme et on la met dans la première*)
    | x :: reste -> (x, (reste, back))
let of_list l = (l, [])
let to_list q = 
  match q with
  | (front, back) -> append_rev front back
;;

(* type 'a t = 'a list (* head of list = first out *)
let empty = []
let push x q = q@[x]
let pop q = match q with x::q' -> x, q' | [] -> raise Not_found
let of_list l = List.rev l
let to_list l = l *)
