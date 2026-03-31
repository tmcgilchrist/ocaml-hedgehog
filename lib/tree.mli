(** Rose tree with lazy children for representing generated values and their
    shrinks.

    A tree [Node (x, children)] represents a generated value [x] together with
    all the ways it can be made smaller. Children should be ordered smallest to
    largest — if the property still fails with the first child, we commit to
    that path without trying the others. *)

type 'a t = Node of 'a * 'a t Seq.t

val value : 'a t -> 'a
(** The generated outcome at the root. *)

val children : 'a t -> 'a t Seq.t
(** The shrink alternatives. *)

val singleton : 'a -> 'a t
(** A tree with no shrinks. *)

val map : ('a -> 'b) -> 'a t -> 'b t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val join : 'a t t -> 'a t
val return : 'a -> 'a t

val fold : ('a -> 'b -> 'c) -> ('c Seq.t -> 'b) -> 'a t -> 'c
(** [fold f g tree] folds over the tree, applying [f] to each node value and the
    result of folding the forest, and [g] to fold a forest. *)

val unfold : ('a -> 'b) -> ('a -> 'a Seq.t) -> 'a -> 'b t
(** [unfold f g x] builds a tree from a value function [f] and a children
    function [g], starting from seed [x]. *)

val unfold_forest : ('a -> 'b) -> ('a -> 'a Seq.t) -> 'a -> 'b t Seq.t

val expand : ('a -> 'a Seq.t) -> 'a t -> 'a t
(** Add additional shrinks to an existing tree via an unfolding function. The
    root outcome remains intact; only additional shrinks are appended. *)

val prune : int -> 'a t -> 'a t
(** [prune n tree] throws away all but the top [n] levels of children. [prune 0]
    removes all children. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Recursively discard shrinks whose outcome does not pass the predicate. The
    root is never discarded. *)

val filter_forest : ('a -> bool) -> 'a t Seq.t -> 'a t Seq.t

val map_option : ('a -> 'b option) -> 'a t -> 'b t option
(** Apply a partial function to a tree. Returns [None] if the root doesn't pass.
    Children that don't pass are removed, but their sub-children that do pass
    are retained. *)

val mzip : 'a t -> 'b t -> ('a * 'b) t
(** Zip two trees together for parallel shrinking. Shrinks try each side
    independently. *)

val interleave : 'a t list -> 'a list t
(** Interleave a list of trees into a tree of lists, with optimal shrinking that
    tries removing chunks and then shrinking individual elements. *)

val cons_child : 'a -> 'a t -> 'a t
(** [cons_child a tree] prepends a singleton child at the origin [a]. *)

val depth : 'a t -> int
(** Returns the depth of the deepest leaf node. *)

val render : ('a -> string) -> 'a t -> string
(** Render a tree as a string for debugging. *)
