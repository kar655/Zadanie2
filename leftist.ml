(** Typ złączalnej kolejki priorytetowej *)
(** wartosc * prawa wysokosc * lewe poddrzewo * prawe poddrzewo **)
type 'a queue =
| Node of 'a * int * 'a queue * 'a queue
| Null

(** Pusta kolejka priorytetowa *)
let (empty: 'a queue) = Null

(* zwraca prawa wysokosc *)
let get_height (q: 'a queue) =
  match q with
  | Null -> 0
  | Node(_, h, _, _) -> h

(** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
let rec join (q1: 'a queue) (q2: 'a queue) =
  match q1, q2 with
  | Null, Null -> Null
  | _, Null -> q1
  | Null, _ -> q2
  | Node(value1, rheight1, l1, r1), Node(value2, rheight2, l2, r2) ->
    if value1 <= value2 then

      let q3 = join r1 q2 in
      let rheight3 = get_height q3 in
      match l1 with
      | Null -> Node(value1, rheight3, q3, Null)
      | Node(_, rheightl1, _, _) ->
        if rheightl1 <= rheight3 then   (* aby zachowac lewicowosc *)
          Node(value1, rheightl1 + 1, q3, l1)
        else
          Node(value1, rheight3 + 1, l1, q3)

    else

      let q3 = join r2 q1 in
      let rheight3 = get_height q3 in
      match l2 with
      | Null -> Node(value2, rheight3, q3, Null)
      | Node(_, rheightl2, _, _) ->
        if rheightl2 <= rheight3 then   (* aby zachowac lewicowosc *)
          Node(value2, rheightl2 + 1, q3, l2)
        else
          Node(value2, rheight3 + 1, l2, q3)

(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e]
    do kolejki [q] *)
let add e (q: 'a queue) =
  join (Node(e, 0, Null, Null)) q

(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] be  z elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min (q: 'a queue) =
  match q with
  | Null -> raise (Empty)
  | Node(x, _, l, r) -> (x, join l r)

(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty (q: 'a queue) =
  q = Null



(*-----------------------Przykladowe testy-----------------------*)

let a = add 10 empty;;
let a = add 12 a;;
let a = add 5 a;;

let b = add 8 empty;;
let b = add 14 b;;
let b = add 7 b;;
let b = add 3 b;;

let c = join a b;;
let (e, c) = delete_min c;;
assert(e = 3);;

let (e, c) = delete_min c;;
assert(e = 5);;

let (e, c) = delete_min c;;
assert(e = 7);;

let (e, c) = delete_min c;;
assert(e = 8);;

let (e, c) = delete_min c;;
assert(e = 10);;

let (e, c) = delete_min c;;
assert(e = 12);;

let (e, c) = delete_min c;;
assert(e = 14);;

assert(is_empty c);;

assert( try let _ = delete_min c in false with Empty -> true);;
