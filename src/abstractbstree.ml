module type AbstractElement =
  sig
    type t
    val compare : t -> t -> int
  end

module type AbstractBST =
  functor (E : AbstractElement) -> sig
    type bstree
    val create  : bstree
    val isEmpty : bstree -> bool
    val add     : bstree -> E.t -> bstree
    val remove  : bstree -> E.t -> bstree
    val search  : bstree -> E.t -> bool
    val min     : bstree -> E.t
    val max     : bstree -> E.t
  end
