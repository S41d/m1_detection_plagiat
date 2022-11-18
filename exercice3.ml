type node = Node of string * node list;;

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

let val_node n = let Node(v, _) = n in v;;

let rec make_tree node_list cl =
  match cl with
  | [] -> []
  | hc::tc ->
    match node_list with
    | [] -> [Node (hc, make_tree [] tc)]
    | hn::tn ->
      let Node(chn, lhn) = hn in
      if chn = hc then Node(hc, make_tree lhn tc)::tn
      else hn::make_tree tn cl
;;

let arbre_suffixe str =
  if not (String.ends_with str ~suffix:"#")
    then failwith "texte invalide"
  else
    let char_list = str_explode str in
    let stree = [] in
    let rec aux tree ct =
      match ct with
      | [] -> tree
      | h::[] -> tree
      | h::t -> aux (make_tree tree ct) t
    in 
    let tree = aux stree char_list in
    tree@[Node ("#", [])];;
;;

let rec tree_has node_list cl =
  match cl with
  | [] -> true
  | hc::tc ->
    match node_list with
    | [] -> false
    | hn::tn ->
      if val_node hn = hc then
        match hn with
        | Node(c, [])-> true
        | Node(c, l) -> tree_has l tc
      else
        tree_has tn cl
;;

let sous_chaine str substr =
  let tree = arbre_suffixe (str^"#") in
  tree_has tree (str_explode substr)
;;

let longest_str str1 str2 =
  if String.length str1 > String.length str2 then str1 else str2
;;

let rec common_substring tree =
    match tree with
    | [] -> ("", false, false)
    | Node(c, l)::t ->
      if c = "#" then
        ("", true, false)
      else if c = "$" then
        ("", false, true)
      else
        let (str_suite, be1_suite, be2_suite) = common_substring l in
        let (str_reste, be1_reste, be2_reste) = common_substring t in
        if be1_suite && be2_suite && be1_reste && be2_reste then
          (c^longest_str str_suite str_reste, true, true)
        else if be1_suite && be2_suite then
          (c^str_suite, true, true)
        else if be1_reste && be2_reste then
          (c^str_reste, true, true)
        else
          (str_reste, (be1_suite || be1_reste), (be2_suite || be2_reste))
;;

let sous_chaine_commune s1 s2 =
  let rec aux tree ct =
    match ct with
    | [] -> tree
    | h::[] -> tree
    | h::t -> aux (make_tree tree ct) t
  in
  let ct2 = str_explode (s2^"$") in
  let tree1 = arbre_suffixe (s1^"#") in
  let tree2 = (aux tree1 (ct2))@[Node ("$", [])] in
  let rec aux2 tree str =
    match tree with
    | [] -> str
    | h::t ->
      let (s, b1, b2) = common_substring tree in
      if b1 && b2 && String.length s > String.length str then
        aux2 t s
      else
        aux2 t str
  in
  let plsc = aux2 tree2 "" in
  (String.length plsc, plsc)
;;

let t = arbre_suffixe "ANANAS#";;

let scc = sous_chaine_commune "ANANAS" "BANANE";;
