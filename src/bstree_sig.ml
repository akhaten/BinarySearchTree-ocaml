module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type BST_sig = sig
    type elt
    (** Type of elements stored in the tree *)
    
    type t
    (** Type of binary search trees *)
    
    val create  : t
    (** Type of binary search trees *)
    
    val is_empty : t -> bool
    (** check if a bst is empty *)

    val get_key : t -> elt

    val get_left : t -> t
    
    val get_right : t -> t

    
    val add     : t -> elt -> t
    (** Insert an element in a tree *)

    val remove  : t -> elt -> t
    (** Remove an element of a tree.
      If the element is already contained, behaves like identity *)

    val search  : t -> elt -> bool
    (** Efficiently search for an element in a tree *)

    val min     : t -> elt
    (** Get the minimal element of a tree according to [E.compare] *)
    
    val max     : t -> elt
    (** Get the maximal element of a tree according to [E.compare] *)
  end
