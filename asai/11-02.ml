(* 目的 : 漸化式 a_0 = 3, a_n = 2 a_{n-1} - 1 の第 n 項を求める *)
(* a : int -> int *)
let rec a n = 
  if n = 0 then 3
  else 2 * a(n - 1) -1

let test1 = a 0 = 3
let test2 = a 1 = 5
let test3 = a 2 = 9
let test4 = a 3 = 17