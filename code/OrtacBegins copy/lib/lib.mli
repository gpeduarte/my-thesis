type 'a t
(*@ mutable model front : 'a list 
    mutable model back : 'a list
    mutable model size : int*)

val is_empty : 'a t -> bool
(*@ b = is_empty q 
    ensures b <-> q.size = 0*)

val make : int -> 'a -> 'a t
(*@ t = make ()
    ensures t.back = [] && t.front = [] && size = 0*)

val pop : 'a t -> 'a
(*@ a = pop t 
    modifies t
    requires List.length t.back >= 0
    ensures a = List.hd t.front && List.length t.front = List.length (old t.front) - 1
    ensures t.front = (old t.front)[1 ..]*)

val push : 'a t -> 'a -> 'a
(*@ push t a
    modifies t
    ensures t.back = a::(old t.back)*)