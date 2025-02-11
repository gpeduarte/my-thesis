include List

type 'a queue = {
  mutable front : 'a list;
  mutable back : 'a list;
  mutable size : int;
}

let[@logic] is_empty q = q.size = 0

let make () =
  { front = []; back = []; size = 0 }

let pop a = 
  let x =
    | [] -> raise Not_found
    | [ x ] ->
        a.front <- List.rev a.rear;
        a.rear <- [];
        x
    | x :: xs ->
        a.front <- xs;
        x
  in
  a.size <- a.size - 1;
  x

let push a x =
  if is_empty a then a.front <- [ x ] else a.rear <- x :: a.rear;
  a.size <- a.size + 1