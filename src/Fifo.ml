(** A FIFO structure (First-In First-Out),
    implemented in functional style
    (NB: the Queue module of OCaml stdlib is imperative)

    NB: l'implémentation fournie initialement ci-dessous est inefficace,
    l'améliorer (tout en restant fonctionnel). Par exemple on peut utiliser
    une paire de listes pour implémenter ['a t].

*)

(* let append left right =
  let rec aux acc l =
    match l with
    | [] -> acc
    | x::r -> aux (x::acc) r
  in aux right (List.rev left) ;;

(* first list is the front of the queue, second list is the back of the queue *)
type 'a t = 'a list * 'a list
let empty = ([], [])
let push x (front, back) = (front, x :: back)
let pop (front, back) = match front with
  | [] -> (match List.rev back with
      | [] -> failwith "empty queue"
      | x :: reste -> (reste, [])) (*si la première liste est vide, on reverse la deuxieme et on la met dans la première*)
  | x :: reste -> (reste, back)
let of_list l = (l, [])
let to_list (front, back) = append front (List.rev back) *)

type 'a t = 'a list (* head of list = first out *)
let empty = []
let push x q = q@[x]
let pop q = match q with x::q' -> x, q' | [] -> raise Not_found
let of_list l = List.rev l
let to_list l = l
