(* 目的 : 整数のリストを受け取ったら、その中の偶数の要素のみを含むリストを返す *)
(* even : int list -> int list *)

let rec even lst = match lst with
| [] ->[]
| head :: rest -> if head mod 2 = 0 then head :: even rest else even rest

(* test *)
let test1 = even [] = [];;
let test2 = even [2] = [2];;
let test3 = even [1;3;5] = [];;
let test4 = even [2;4;6] = [2;4;6];;