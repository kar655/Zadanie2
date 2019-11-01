(** Typ złączalnej kolejki priorytetowej *)
type 'a queue =
| Node of 'a * 'a queue * 'a queue
| Null

let a = Node(5, Node(10, Null, Null), Node(12, Null, Null))
let b = Node(3, Node(7, Node(14, Null, Null), Null), Node(8, Null, Null))

(** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
exception Blad

let rec join (q1: 'a queue) (q2: 'a queue) =
  match q1, q2 with
  | Null,Null -> Null
  | q1, Null -> q1
  | Null, q2 -> q2
  | Node(value1, llh1, rlh1), Node(value2, llh2, rlh2) ->
    if value1 < value2 then
      Node(value1, llh1, join rlh1 q2 )
    else if value1 > value2 then
      Node(value2, llh2, join rlh2 q1)
    else raise (Blad)

(** Pusta kolejka priorytetowa *)
let (empty: 'a queue) = Null

(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e]
    do kolejki [q] *)
let add e (q: 'a queue) =
  join (Node(e, Null, Null)) q

(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] be  z elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min (q: 'a queue) =
  match q with
  | Null -> raise (Empty)
  | Node(x, l, r) -> (x, join l r)



(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty (q: 'a queue) =
  if q = Null then true else false
