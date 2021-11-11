(* 目的 : 文字列のリストを受け取ったら、その中の要素をくっつけた文字列を返す *)
(* concat : string list -> string *)

let rec concat lst = match lst with
| [] -> ""
| head :: rest -> head ^ concat rest

(* test *)
let test1 = concat [] = "";;
let test2 = concat [""] = "";;
let test3 = concat ["a"] = "a";;
let test4 = concat ["a";"b"] = "ab";;