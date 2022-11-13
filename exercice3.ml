type stree =
  | Tree of stree list
  | Node of string * stree list
  | Leaf of string
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

let longest_str l =
  let rec aux max l =
    match l with
    | [] -> max
    | h::t ->
      begin
        let str_len = String.length h in
        if str_len > max then aux str_len t
        else aux max t
      end
  in aux 0 l
;;

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
        if hc = "#" then [Leaf "#"]
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
    | Tree l -> Tree(l@[Leaf "#"])
    | _ -> failwith "not a tree(arbre_suffixe)";;
;;

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
    | Node (c, l) -> c = str_implode char_list && tree_has_list l char_list
    | Leaf c -> c = str_implode char_list
;;

let sous_chaine str substr =
  let tree = arbre_suffixe str in
  tree_has tree (str_explode substr)
;;

let sous_chaine_commune s1 s2 =
  let rec aux tree ct =
    let rec aux2 tree ct =
      match ct with
      | [] -> tree
      | h::[] -> tree
      | h::t -> aux2 (make_tree tree ct) t
    in
    let st = aux2 tree ct in
    match st with
    | Tree l -> Tree(l@[Leaf "#"])
    | _ -> failwith "not a tree (aux scc)"
  in
  let ct2 = str_explode (s2) in
  let tree1 = arbre_suffixe (s1^"#") in
  let tree2 = aux tree1 ct2 in
  let rec parcours tree =
    let rec parcours_liste l =
      match l with
      | [] -> (0, [])
      | h::t -> 
        let (l1, r1) = parcours h in
        if tree_has h r1 then
          let (l2, r2) = parcours_liste t in
          if l1 > l2 then 
            (l1, r1)
          else 
            (l2, r2)
        else (l1, r1)
    in
    match tree with
    | Leaf c -> (0, [])
    | Node (c, l) ->
      if tree_has tree (str_explode c) then
        let (count, cl) = (parcours_liste l) in
        (count+1, c::cl)
      else parcours_liste l
    | Tree l -> parcours_liste l
  in parcours tree2
;;

let t = arbre_suffixe "ANANAS#";;

let scc = sous_chaine_commune "ANANAS" "BANANE";;

let scc = sous_chaine_commune "ABA" "CACAB";;
