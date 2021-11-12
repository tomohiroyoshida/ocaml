(* 目的 : 0から受け取った自然数までの2乗の和を求める *)
(* sum : int -> int *)
let rec sum n = 
  if n = 0 then 0
  else n * n + sum (n - 1)

(* テスト用の関数 *)
let f n = n * (n + 1) * (2 * n + 1) / 6
(* テスト *)
let test1 = sum 0 = f 0
let test2 = sum 1 = f 1
let test3 = sum 2 = f 2
let test4 = sum 4 = f 4
let test5 = sum 100 = f 100