type stree =
  | Tree of stree list
  | Node of string * stree list
  | Leaf of string
;;
open Exercice3;;

let val_node n =
  match n with
  | Leaf v -> v
  | Node(v, _) -> v
  | Tree l -> failwith "failed"
;;

let rec compression node =
  let rec compression_list l =
    match l with
    | [] -> []
    | h::t -> (compression h)::(compression_list t)
  in match node with
  | Leaf c -> node
  | Tree l -> Tree(compression_list l)
  | Node(c, l) -> 
    match l with
    | h::[] -> 
      begin match h with
        | Node (c2, l2) -> 
          let com_list = compression_list l2 in
          if (List.length com_list) = 1 then
            Leaf(c^val_node (compression h))
          else 
            Node(c^val_node (compression h), com_list)
        | Leaf c2 when c2 = "#" -> Leaf (c^c2)
        | _ -> failwith "should not be tree"
      end
    | _::_ -> Node(c, compression_list l)
    | _ -> failwith "not a node"
;;

let compresse = compression t;;
