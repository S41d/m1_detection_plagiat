let rec move_list liste i count = 
  match liste with 
  | [] -> [] 
  | h::t -> 
    if i < count then 
      move_list t (i + 1) count
    else liste
;;

let rec length_str t1 i t2 j =
  let text_1 = move_list t1 0 i in
  let text_2 = move_list t2 0 j in
  match (text_1, text_2) with
  | (h1::t1, h2::t2) when h1 = h2 -> 1 + (length_str t1 0 t2 0) 
  | _ -> 0
;; (* O(n) *)

let text_1  = ["A"; "N"; "A"; "N"; "A"; "S"];;
let text_2  = ["B"; "A"; "N"; "A"; "N"; "E"];;

length_str text_1 0 text_2 1;;

let plsc x y = 
  let nx = List.length x in
  let ny = List.length y in
  let m = ref 0 in
  let pos_x = ref 0 in
  let pos_y = ref 0 in
  for i = 0 to nx - 1 do
    for j = 0 to ny - 1 do
      let len = (length_str x i y j) in
      if !m < len then 
      begin
        m := len;
        pos_x := i;
        pos_y := j;
      end
    done;
  done;
  (!m, (!pos_x, !pos_y))
;; (* O(n3) *)

let test = plsc text_1 text_2;;
