(* 4.1 *)
let baito_kyuryo x y = (850+(x*100))*y;;

(* 4.3 *)
let hyojun_taiju m = m**2.*.22.;;

(* 4.4 *)
let bmi m kg = kg/.m**2.;;

(* 4.6 *)
(* 引数に匹（num）を与えると、つるの足の数を数える *)
(*tsuru: int -> int *)
let tsuru num = num * 2;;

(* test *)
let t1 = tsuru 1 = 2;;
let t2 = tsuru 2 = 4;;
let t3 = tsuru 10 = 20;;

(* 4.8 *)
(* 鶴と亀の数の合計(x)と足の数の合計(y)を与えられたら、鶴の数を返す関数 *)
(* int -> int -> int *)
let tsurukame x y = x - ((y - (x*2)) / 2);;

let t4 = tsurukame 100 274 = 63;;
let t5 = tsurukame 1 2 = 1;;
let t6 = tsurukame 2 8 = 0;;