
let rec fold_left f acc l = match l with
  | [] -> acc
  | x :: r -> fold_left f (f acc x) r

let plus x y = x + y

let x = fold_left plus 0 [1;2;3;4]
let _ = print_int x
let _ = print_newline ()

let _ = print_int (fold_left plus 42 [])
let _ = print_newline ()
