(** Typ złączalnej kolejki priorytetowej *)
type 'a queue =
| Node of 'a * int * 'a queue * 'a queue
| Null of int

(** Pusta kolejka priorytetowa *)
let (empty: 'a queue) = Null(-1)

let a = Node(5,1,Node(10,0,empty,empty),Node(12,0,empty,empty))
let b = Node(3, 1, Node(7,0,Node(14,0,empty,empty),empty), Node(8,0,empty,empty))

(** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
exception Blad


let rec join (q1: 'a queue) (q2: 'a queue) =
  match q1, q2 with
  | Null(_), Null(_) -> empty
  | q1, Null(_) -> q1
  | Null(_), q2 -> q2
  | Node(value1, rheight1, l1, r1), Node(value2, rheight2, l2, r2) ->
    if value1 <= value2 then   (* Node(value1, l1, join rlh1 q2 ) *)
      (* if npl(h1)  *)
      let Node(_, rheight3, _, _) as q3 = join r1 q2 in

      if l1 = empty then Node(value1, rheight3, q3, empty)
      else

        let Node(_, rheightl1, _, _) = l1 in

        if rheightl1 <= rheight3 then
          Node(value1, rheightl1, q3, l1)
        else
          Node(value1, rheight3, l1, q3)

    else
    
      let Node(_, rheight3, _, _) as q3 = join r2 q1 in
      if l2 = empty then Node(value2, rheight3, q3, empty)
      else
        let Node(_, rheightl2, _, _) = l2 in

        if rheightl2 <= rheight3 then
          Node(value2, rheightl2, q3, l2)
        else
          Node(value2, rheight3, l2, q3)

(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e]
    do kolejki [q] *)
let add e (q: 'a queue) =
  join (Node(e, 0, empty, empty)) q

(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] be  z elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min (q: 'a queue) =
  match q with
  | Null(_) -> raise (Empty)
  | Node(x, _, l, r) -> (x, join l r)



(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty (q: 'a queue) =
  if q = empty then true else false
