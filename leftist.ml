(** Typ złączalnej kolejki priorytetowej *)
(** wartosc * prawa wysokosc * lewe poddrzewo * prawe poddrzewo **)
type 'a queue =
| Node of 'a * int * 'a queue * 'a queue
| Null

(** Pusta kolejka priorytetowa *)
let (empty: 'a queue) = Null

(** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
let rec join (q1: 'a queue) (q2: 'a queue) =
  match q1, q2 with
  | Null, Null -> Null
  | q1, Null -> q1
  | Null, q2 -> q2
  | Node(value1, rheight1, l1, r1), Node(value2, rheight2, l2, r2) ->
    if value1 <= value2 then
      (* gdyby Null = join r1 q2 to r1 i q2 musialoby byc Null
      ale gdyby q2 bylo Null to wczesniejscy match by sie odpalil *)
      let Node(_, rheight3, _, _) as q3 = join r1 q2 in
      match l1 with
      | Null -> Node(value1, rheight3, q3, Null)
      | Node(_, rheightl1, _, _) ->
        if rheightl1 <= rheight3 then   (* aby zachowac lewicowosc *)
          Node(value1, rheightl1, q3, l1)
        else
          Node(value1, rheight3, l1, q3)

    else
      (* gdyby Null = join r2 q1 to r2 i q1 musialoby byc Null
      ale gdyby q1 bylo Null to wczesniejscy match by sie odpalil *)
      let Node(_, rheight3, _, _) as q3 = join r2 q1 in
      match l2 with
      | Null -> Node(value2, rheight3, q3, Null)
      | Node(_, rheightl2, _, _) ->
        if rheightl2 <= rheight3 then   (* aby zachowac lewicowosc *)
          Node(value2, rheightl2, q3, l2)
        else
          Node(value2, rheight3, l2, q3)

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
