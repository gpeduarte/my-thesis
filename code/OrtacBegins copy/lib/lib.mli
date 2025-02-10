type 'a t
(*@ mutable model front : 'a list 
    mutable model back : 'a list*)

val make : int -> 'a -> 'a t
(*@ t = make i a 
    ensures List.length t.back = 0 && List.length t.front = 0*)

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