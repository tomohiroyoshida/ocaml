type ekimei_t = { 
  kanji   : string; (* 駅名 *) 
  kana    : string; (* 読み *) 
  romaji  : string; (* ローマ字 *) 
  shozoku : string; (* 所属線名 *) 
  } 
let lst1 = []
let lst2 = [{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}]
let lst3 = [{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"};
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}]

(* 目的 : ekimei_t 型のかなの昇順に並んだリストとアイテムを受け取ったら、アイテムをかなの昇順に挿入する *)
(* eki_insert : ekimei_t list -> ekimei_t -> ekimei_t list *)
let rec eki_insert lst x = match lst with
| [] -> [x]
| ({kanji;kana;romaji;shozoku} as first) :: rest -> 
  if x.kana < first.kana then x :: lst
  else first :: eki_insert rest x
  
(* test *)
let test1 = eki_insert lst1 {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"} 
  = [{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}]
let test2 = eki_insert lst2 {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}
  = [{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
     {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}]

(* ekimei_t のリストを受け取ってかなの昇順に並べ替える *)
(* eki_sort : ekimei_t list -> ekimei_t list *)
let rec eki_sort lst = match lst with
| [] -> []
| ({kanji;kana;romaji;shozoku} as first) :: rest ->
  eki_insert (eki_sort rest) first

(* test *)
let test1 = eki_sort lst1 = []
let test2 = eki_sort lst3 = [{kanji = "明治神宮前"; kana = "めいじじんぐうまえ";
                              romaji = "meijijinguumae"; shozoku = "千代田線"};
                            {kanji = "明治神宮前"; kana = "めいじじんぐうまえ";
                              romaji = "meijijinguumae"; shozoku = "千代田線"};
                            {kanji = "代々木上原"; kana = "よよぎうえはら";
                              romaji = "yoyogiuehara"; shozoku = "千代田線"};
                            {kanji = "代々木公園"; kana = "よよぎこうえん";
                              romaji = "yoyogikouen"; shozoku = "千代田線"}]

(* 目的 : ekimei_t 型のかなのリストを受け取ったら、駅の重複を取り除いたリストを返す *)
(* seiretsu : ekimei_t list -> ekimei_t list *)
let rec eki_deduplicate lst = match lst with
| [] -> []
| [x] -> [x]
| first :: second :: rest ->
  if first.kana = second.kana then eki_deduplicate (second :: rest)
  else first :: eki_deduplicate (second :: rest)

let test1 = eki_deduplicate [] = []
let test2 = eki_deduplicate lst3 = [{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}]

(* 目的 : ekimei_t 型のリストを受け取ったら、それをひらがなの順に整列し、駅の重複を取り除いたリストを返す *)
(* seiretsu : ekimei_t list -> ekimei_t list *)
let rec eki_sort_deduplicate lst = 
  let sorted_list = eki_sort lst in
  eki_deduplicate sorted_list

let test1 = eki_sort_deduplicate lst1 = lst1
let test2 = eki_sort_deduplicate lst2 = lst2
let test3 = eki_sort_deduplicate lst3 = [{kanji = "明治神宮前"; kana = "めいじじんぐうまえ";
                                          romaji = "meijijinguumae"; shozoku = "千代田線"};
                                         {kanji = "代々木上原"; kana = "よよぎうえはら";
                                          romaji = "yoyogiuehara"; shozoku = "千代田線"};
                                         {kanji = "代々木公園"; kana = "よよぎこうえん";
                                          romaji = "yoyogikouen"; shozoku = "千代田線"}]