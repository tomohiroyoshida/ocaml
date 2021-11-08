type person_t = {
  name : string;          (* 名前 *)
  height : float;         (* 身長(m) *)
  weight : float;         (* 体重(kg) *)
  birthday : int * int;   (* 誕生日（月と日） *)
  bloodtype : string;     (* 血液型 *)
}

let person1 = {
  name = "一郎";
  height = 170.0;
  weight = 60.0;
  birthday = (6,2);
  bloodtype = "A";
}

let person2 = {
  name = "二郎";
  height = 168.0;
  weight = 56.0;
  birthday = (11,26);
  bloodtype = "O";
}

let person3 = {
  name = "三郎";
  height = 148.0;
  weight = 45.0;
  birthday = (1,1);
  bloodtype = "AB";
}

let lst1 = []
let lst2 = [person1]
let lst3 = [person1; person2]
let lst4 = [person1; person2; person3]

(* 目的 : person_t 型のリストを受け取り、血液型がA型の人の人数を返す *)
(* count_ketsueki_A : person_t list -> int *)

let rec num lst = match lst with
| [] -> 0
| { name = n; height = h; weight = w; birthday = b; bloodtype = bt } :: rest ->
  if bt = "A" then 1 + num rest else num rest

(* test *)
let test1 = num [] = 0;;
let test2 = num [person1] = 1;;
let test3 = num [person2] = 0;;
let test4 = num[person1;person2;person3] = 1;;