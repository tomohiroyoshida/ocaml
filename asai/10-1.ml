(* 目的 : 昇順に並んでいる整数のリストlstと整数nを受け取ったら、昇順になるようにlstにnを挿入したリストを作る *)
(* insert : int list -> int -> int list *)
let rec insert lst n = match lst with
  | [] -> [n]
  | head :: rest -> if head < n then head :: insert rest n else n :: head :: rest

(* test *)
let test1 = insert [] 1 = [1];;
let test2 = insert [1] 2 = [1;2];;
let test3 = insert [3] 1 = [1;3];;
let test4 = insert [1;2;4] 3 = [1;2;3;4];;