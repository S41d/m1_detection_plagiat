type stree =
  | Tree of stree list
  | Node of string * stree list
  | Leaf of string
;;

let val_node n =
  match n with
  | Leaf v -> v
  | Node(v, _) -> v
  | Tree l -> failwith "failed"
;;

let str_explode s =
  let char_list = List.init (String.length s) (String.get s) in
  let rec aux ch =
    match ch with
    | [] -> []
    | h::t -> (String.make 1 h)::aux t
  in aux char_list
;;

let rec str_implode cl =
  match cl with
  | [] -> ""
  | h::t -> h^str_implode t
;;

(* debut exercice 5 *)
let rec prefixes l1 l2 = 
  match l1, l2 with
  | h1::t1, h2::t2 -> if h1 = h2 then h1::prefixes t1 t2 else []
  | [], _ -> []
  | _, [] -> []
;;

let substr_node node str prefixe = 
  let vnode = val_node node in
  let lenp = List.length prefixe in
  let vnew_node = str_implode prefixe in
  let ssold = String.sub vnode lenp (String.length vnode - lenp) in
  let ssnew = String.sub str lenp (String.length str - lenp) in
  match node with
  | Leaf c -> Node(vnew_node, [Leaf ssold; Leaf ssnew])
  | Node(c, l) -> Node(vnew_node, [Node(ssold, l); Leaf ssnew])
  | Tree l -> failwith "bruh"
;;

let rec make_comp_tree node char_list =
  let rec make_tree_list node_list cl =
    let str = str_implode cl in
    match cl with
    | [] -> []
    | hc::tc ->
      match node_list with
      | [] -> [Leaf str]
      | hn::tn ->
        let p = prefixes (str_explode (val_node hn)) cl in
        if List.length p != 0 then
          substr_node hn (str_implode cl) p::tn
        else
          hn::make_tree_list tn cl
  in
  match char_list with
  | [] -> failwith "charlist empty"
  | hc::tc ->
    match node with
    | Leaf _ -> node
    | Tree l -> Tree(make_tree_list l char_list)
    | Node(c, l) -> Node(c, make_tree_list l tc)
;;

let arbre_suffixes_compresse str = 
  let char_list = str_explode (str^"#") in
  let stree = Tree([]) in
  let rec aux tree ct =
    match ct with
    | [] -> tree
    | h::[] -> tree
    | h::t -> aux (make_comp_tree tree ct) t
  in aux stree char_list
;;
