(* 学生一人分のデータを表す型 *)
type gakuset_t = {
  namae : string;   (* 名前 *)
  tensuu : int;     (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* gakusei_t list は
    - []                空リスト、あるいは
    - first :: rest )   最初の要素が firstで残りのリストがrest
                        (first は gakusei_t 型
                        rest が自己参照のケース 
  という形。 *)
  (* galisei_t list 型のデータの例 *)
let lst1 = []
let lst2 = [{namae = "asai"; tensuu = 70; seiseki = "B"}]
let lst3 = [{namae = "asai"; tensuu = 70; seiseki = "B"};
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}]
let lst4 = [{namae = "yoshida"; tensuu = 80; seiseki = "A"};
            {namae = "asai"; tensuu = 70; seiseki = "B"};
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}]

(* 目的 : gakusei_t型の昇順のリストとgakusei_t型のデータを受け取ったら、それを挿入した昇順のリストを返す *)
(* gakusei_insert : gakusei_t list -> galiset_t -> gakusei_t list *)
let rec insert lst x = match lst with
| [] -> [x]
| ({namae = n; tensuu = t; seiseki = s} as first) :: rest -> 
  if t < x.tensuu then first :: (insert rest x) else x :: lst

let test1 = insert lst1 {namae = "asai"; tensuu = 70; seiseki = "B"} = lst2;;
let test2 = insert lst2 {namae = "kaneko"; tensuu = 85; seiseki = "A"} = lst3;;

(* 目的 : gakusei_t型のリストを受け取ったら、点数の順に整列したリストを返す *)
(* gakusei_sort : gakusei_t list -> gakusei_t list *)
let rec sort lst = match lst with
| [] -> []
| ({namae = n; tensuu = t; seiseki = s} as first) :: rest ->
  insert (sort rest) first

let test3 = sort [] = [];;
let test4 = sort lst4 = [
                          {namae = "asai"; tensuu = 70; seiseki = "B"};
                          {namae = "yoshida"; tensuu = 80; seiseki = "A"};
                          {namae = "kaneko"; tensuu = 85; seiseki = "A"}
                        ]