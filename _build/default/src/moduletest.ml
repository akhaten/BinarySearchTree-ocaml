open Bstree

module INT = struct 
    type t = int 
    let compare a b = a - b
  end

module BSTINT = BST(INT) ;;

