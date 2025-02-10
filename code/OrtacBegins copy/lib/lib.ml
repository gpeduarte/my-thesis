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
  a.back <- x::a.back

let reverse array =
  let len = List.length array in
  for i = 0 to (len / 2) - 1 do 
    let temp = array.(i) in
    array.(i) <- array.(len - i - 1);
    array.(len - i - 1) <- temp
  done