open Bstree_sig

module BST (E : Comparable ) : BST_sig with type elt = E.t = struct

  type elt = E.t
  
  type t =
    | NIL
    | BSTree of t * elt * t


  (* PRIVATE FUNCTIONS *)

  (* let bstree_isLeaf bst = 
    match bst with
    | BSTree(NIL, _, NIL) -> true
    | _ -> false *)
    
  let get_left = function
    | NIL -> failwith "bstree_left : the bstree is a leaf"
    | BSTree(left, _, _) -> left

  let get_right = function
    | NIL -> failwith "bstree_right : the bstree is a leaf"
    | BSTree(_, _, right) -> right

  let get_key = function 
    | NIL -> failwith "bstree_getkey : the bstree is a leaf"
    | BSTree(_, key, _) -> key
    
  let rec min = function
    | NIL -> failwith "min : the bstree is a leaf"
    | BSTree(left, key, _) -> 
      if left = NIL then key else min left

  let rec max = function
    | NIL -> failwith "min : the bstree is a leaf"
    | BSTree(_, key, right) -> 
      if right = NIL then key else max right
    
    
  let _bstree_successor bst = min (get_right bst)

  let _bstree_ancestor bst = max (get_left bst)

  let _rotate_right bst =
    match bst with
    | NIL | BSTree (NIL, _, _) -> failwith "bstree_rotate_right : right is leaf"
    | BSTree (BSTree (alpha, key, beta), value, right) ->
        BSTree (alpha, key, BSTree (beta, value, right))

  let _rotate_left bst =
    match bst with
    | NIL | BSTree (_, _, NIL) -> failwith "bstree_rotate_left : left is leaf"
    | BSTree (left, value, BSTree (alpha, key, beta)) ->
      BSTree (BSTree (left, value, alpha), key, beta)

  (* PUBLIC FUNCTIONS *)
  
  let create = NIL

  let is_empty bst = bst = NIL

  let rec add bst value =
    match bst with
    | NIL -> BSTree(NIL, value, NIL)
    | BSTree(left, key, right) ->
      if value = key then bst
      else if value < key then
        BSTree(add left value, key, right)
      else
        BSTree(left, key, add right value)

  let rec remove bst value =
    match bst with
    | NIL -> NIL
    | BSTree (NIL, key, NIL) -> if value = key then NIL else bst
    | BSTree (left, key, right) ->
      if E.compare value key = 0 then
        if right = NIL then
          (* at this point, left cant be NIL *)
          let key' = max left in
          BSTree (remove left key', key', right)
        else
          (* at this point, right cant be NIL *)
          let key' = min right in
          BSTree (left, key', remove right key')
      else if E.compare value key < 0 then
        (* if [value] is in the tree, it belongs to [left] *)
        BSTree (remove left value, key, right)
      else
        (* if [value] is in the tree, it belongs to [right] *)
        BSTree (left, key, remove right value)

  let rec search bst value =
    match bst with
    | NIL -> false
    | BSTree (left, key, right) ->
      E.compare key value = 0 || search left value || search right value
end
