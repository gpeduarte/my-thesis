type 'a t
(*@ model size : int
    mutable model contents : 'a list *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.size = i
    ensures t.contents = List.init i (fun j -> a) *)

val get : 'a t -> int -> 'a
(*@ a = get t i
    checks 0 <= i < t.size
    ensures a = List.nth t.contents i *)

val set : 'a t -> int -> 'a -> unit
(*@ set t i a
    checks 0 <= i < t.size
    modifies t.contents
    ensures t.contents = List.mapi (fun j x -> if j = (i : integer) then a else x) (old t.contents) *)