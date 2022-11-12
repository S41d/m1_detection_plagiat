type stree =
  | Tree of stree list
  | Node of char * stree list
  | Leaf of char
;;
exception NotTree of string;;

let str_explode s = s |> String.to_seq |> List.of_seq ;;

let val_node n =
  match n with
  | Leaf v -> v
  | Node(v, _) -> v
  | Tree l -> failwith "failed"
;;

let rec make_tree node char_list =
  let rec make_tree_list node_list cl =
    match cl with
    | [] -> []
    | hc::tc ->
      match node_list with
      | [] -> 
        if hc = '#' then [Leaf '#']
        else [Node (hc, make_tree_list [] tc)]
      | hn::tn ->
        if val_node hn = hc then (make_tree hn cl)::tn
        else hn::make_tree_list tn cl
  in
  match char_list with
  | [] -> failwith "charlist empty"
  | hc::tc ->
    match node with
    | Leaf _ -> node
    | Tree l -> Tree(make_tree_list l char_list)
    | Node(c, l) -> Node(c, make_tree_list l tc)
;;

let arbre_suffixe str =
  if not (String.ends_with str ~suffix:"#")
    then failwith "texte invalide"
  else
    let char_list = str_explode str in
    let stree = Tree([]) in
    let rec aux tree ct =
      match ct with
      | [] -> tree
      | h::[] -> tree
      | h::t -> aux (make_tree tree ct) t
    in 
    let st = aux stree char_list in
    match st with
    | Tree l -> Tree(l@[Leaf '#'])
    | _ -> failwith "not a tree";;
;;

let t = arbre_suffixe "ANANAS#";;

let rec tree_has node char_list =
  let rec tree_has_list node_list cl = 
    match cl with
    | [] -> true
    | hc::tc ->
      match node_list with
      | [] -> false
      | hn::tn ->
        if val_node hn = hc then 
          match hn with
          | Leaf c -> true
          | Node(c, l) -> tree_has_list l tc
          | Tree _ -> failwith "tree should be node"
        else 
          tree_has_list tn cl
  in
  match node with
    | Tree l -> tree_has_list l char_list
    | _ -> failwith "not a tree";
;;

let sous_chaine str substr =
  let tree = arbre_suffixe str in
  tree_has tree (str_explode substr)
;;

let sc = sous_chaine "ANA#" "NA";;