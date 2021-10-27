type ekimei_t = {
  kanji: string;
  kana: string;
  romaji: string;
  shozoku: string;
};;

(* ekimei_tをとって「路線名、駅名（かな）を返す関数 *)
(* ekimei_t -> string *)
let hyoji x = match x with 
  { kanji=kanji; kana=kana; romaji=romaji; shozoku=shozoku} ->
    shozoku ^ "、" ^ kanji ^ "（" ^ kana ^ "）";;

(* test *)
let test1 = hyoji {kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線"}
          = "丸ノ内線、茗荷谷（みょうがだに）"
let test2 = hyoji { kanji = "富山"; kana = "とやま"; romaji = "toyama"; shozoku = "あいの風" }
          = "あいの風、富山（とやま）";;