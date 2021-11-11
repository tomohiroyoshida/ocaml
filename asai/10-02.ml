(* 目的 : 昇順に並んでいる整数のリストlstと整数nを受け取ったら、昇順になるようにlstにnを挿入したリストを作る *)
(* insert : int list -> int -> int list *)
let rec insert lst n = match lst with
  | [] -> [n]
  | head :: rest -> if head < n then head :: insert rest n else n :: head :: rest

(* 目的 : 整数のリストを受け取ったら、それを昇順に整列したリストを返す *)
(* ins_sort : int list -> int list *)
let rec sort lst = match lst with
  | [] -> []
  | first :: rest -> insert (sort rest) first

(* test *)
let test1 = sort [] = [];;
let test2 = sort [2;3;4] = [2;3;4];;
let test3 = sort [4;3;2] = [2;3;4];;