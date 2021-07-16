open Abstractbstree

module BST : AbstractBST =
  functor (E : AbstractElement) -> struct

    type bstree =
      | NIL
      | BSTree of bstree * E.t * bstree
                    
    
    let bstree_isLeaf bst = 
      match bst with
      | NIL | BSTree(NIL, _, NIL) -> true
      | _ -> false 
      
    
    let bstree_left = function
      | NIL -> failwith "bstree_left : the bstree is a leaf"
      | BSTree(left, _, _) -> left
      
    let bstree_right = function
      | NIL -> failwith "bstree_right : the bstree is a leaf"
      | BSTree(_, _, right) -> right
        
    let bstree_getkey = function 
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
              
    let bstree_successor bst = min (bstree_right bst)

    let bstree_ancestor bst = max (bstree_left bst)
                                                                 
    let bstree_rotate_right bst =
      let bstree_setLeft bst left =
        match bst with
        | NIL -> failwith "bstree_setLeft : the bstree is a leaf"
        | BSTree(_, value, right) -> BSTree(left, value, right)
      in match bst with
      | NIL | BSTree(NIL, _, _) -> failwith "bstree_rotate_right : right is leaf"
      | _ -> let left = bstree_left bst in
          match left with
          | NIL -> failwith "bstree_rotate_right : right is leaf"
          | BSTree(alpha, key, beta) -> BSTree(alpha, key, (bstree_setLeft bst beta)) 
                            
    let bstree_rotate_left bst =
      let bstree_setRight bst right =
        match bst with
        | NIL -> failwith "bstree_setRight : the bstree is a leaf"
        | BSTree(left, value, _) -> BSTree(left, value, right)
      in match bst with
      | NIL | BSTree(_, _, NIL) -> failwith "bstree_rotate_left : left is leaf"
      | _ -> let right = bstree_right bst in
          match right with
          | NIL -> failwith "bstree_rotate_left : right is leaf"
          | BSTree(beta, key, gamma) -> BSTree((bstree_setRight bst beta), key, gamma)             
              
    let create = NIL

    let isEmpty bst = bst = NIL

    let rec add bst value =
      match bst with
      | NIL -> BSTree(NIL, value, NIL)
      | BSTree(left, key, right) ->
          if value = key then
            bst
          else if value < key then
            BSTree((add left value), key, right)
          else
            BSTree(left, key, (add right value))

    let rec remove bst value =
      match bst with
      | NIL -> failwith "bstree_remove : the bstree is a leaf" 
      | BSTree(left, key, right) ->
          if value = key then
            if bstree_isLeaf bst then
              NIL
            else
            if right = NIL then 
              let newkey = bstree_ancestor bst in
              BSTree((remove left newkey), newkey, right)
            else
              let newkey = bstree_successor bst in
              BSTree(left, newkey, (remove right newkey))
          else if value < key then
            BSTree((remove left value), key, right)
          else
            BSTree(left, key, (remove right value))
            
    let rec search bst value = 
      match bst with
      | NIL -> false
      | BSTree(left, key, right) -> 
          ((E.compare key value) = 0) || (search left value) || (search right value)


  end
