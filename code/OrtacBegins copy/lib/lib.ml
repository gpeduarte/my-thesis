include List

type 'a queue = {
  mutable front : 'a list;
  mutable back : 'a list;
}

let make () =
  { front = []; back = [] }

let pop a = 
  match a.front with
  | [] -> (match List.rev a.back with
          | [] -> failwith "empty"
          | x::xs -> a.front <- xs; a.back <- []; Some x)
  | x::xs -> a.front <- xs; Some x

let push a x =
  a.back <- x::a.back

let reverse array =
  let len = List.length array in
  for i = 0 to (len / 2) - 1 do 
    let temp = array.(i) in
    array.(i) <- array.(len - i - 1);
    array.(len - i - 1) <- temp
  done