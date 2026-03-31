(** Generators for random values with integrated shrinking.

    A generator takes a size parameter and a seed, and produces a shrink tree
    where the root is the generated value and children are shrunk alternatives.

    The [option] in the return type supports discarding: [None] means the
    generated value was discarded (e.g. by {!filter}). *)

type 'a t = int -> Seed.t -> 'a Tree.t option

(** {2 Monad} *)

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val apply : ('a -> 'b) t -> 'a t -> 'b t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

(** {2 Numeric generators} *)

val integral : int Range.t -> int t
(** Generate a random integer in the given range, with shrinking towards the
    range's origin via binary search. *)

val int : int Range.t -> int t
(** Alias for {!integral}. *)

val float : float Range.t -> float t
(** Generate a random float in the given range, shrinking towards the range's
    origin. *)

val bool : bool t
(** Generate a random boolean, shrinking towards [false]. *)

(** {2 Characters & strings} *)

val char : int Range.t -> char t
(** Generate a character from a range of character codes. *)

val digit : char t
val lower : char t
val upper : char t
val alpha : char t
val alpha_num : char t
val ascii : char t

val string : int Range.t -> char t -> string t
(** Generate a string using a range for the length and a char generator. *)

(** {2 Choice combinators} *)

val element : 'a list -> 'a t
(** Randomly select an element from a list. Shrinks towards the first element.
*)

val choice : 'a t list -> 'a t
(** Randomly select a generator from a list. Shrinks towards the first
    generator. *)

val frequency : (int * 'a t) list -> 'a t
(** Use weighted distribution to select a generator. Shrinks towards generators
    with smaller indices. *)

val recursive : ('a t list -> 'a t) -> 'a t list -> 'a t list -> 'a t
(** [recursive f nonrec rec_] selects from [nonrec] and [rec_] generators. When
    size <= 1, only [nonrec] generators are used. Recursive generators have
    their size halved. *)

(** {2 Collections} *)

val list : int Range.t -> 'a t -> 'a list t
(** Generate a list using a range for the length. Uses {!Tree.interleave} for
    optimal shrinking. *)

val non_empty : int Range.t -> 'a t -> 'a list t
(** Generate a non-empty list. The first element is always present; the rest
    uses [list] with the given range. Shrinks will never produce [[]]. *)

val shuffle : 'a list -> 'a list t
(** Generate a random permutation of a list. Shrinks towards the original order.
*)

val subsequence : 'a list -> 'a list t
(** Generate a random subsequence of a list, preserving order. Shrinks towards
    [[]]. *)

val option : 'a t -> 'a option t
(** Generates [None] some of the time. *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
(** Generate either a [Left] or [Right] value with 50/50 probability. *)

val unique : ('a -> 'a -> int) -> int Range.t -> 'a t -> 'a list t
(** Generate a list of unique elements (no duplicates according to the given
    comparison function). Shrinks via the underlying list shrinking. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** Generate a pair with parallel shrinking. *)

(** {2 Freeze} *)

val freeze : 'a t -> ('a * 'a t) t
(** [freeze gen] captures the generator's output. Returns a pair of the
    generated value and a frozen generator that always produces that value. *)

(** {2 Integer width variants} *)

val int32 : int32 Range.t -> int32 t
(** Generate a random [int32] in the given range, with shrinking towards the
    origin. Converts through [int] internally (safe on 64-bit OCaml). *)

val int64 : int64 Range.t -> int64 t
(** Generate a random [int64] in the given range, with shrinking towards the
    origin. Uses native [int64] arithmetic. *)

(** {2 Subterm combinators}

    Generate values that shrink to structural subterms. Useful for recursive
    data types (ASTs, trees, JSON, etc.): if [Neg(e)] fails, the shrinker tries
    [e] directly before trying [Neg(e')] for smaller [e']. *)

val subterm : 'a t -> ('a -> 'a) -> 'a t
(** [subterm gen f] generates a value by applying [f] to a value from [gen].
    Shrinks include the raw subterm at every level. *)

val subterm2 : 'a t -> 'a t -> ('a -> 'a -> 'a) -> 'a t
(** [subterm2 g1 g2 f] generates [f x y] from [g1] and [g2]. Shrinks include
    both raw subterms. *)

val subterm3 : 'a t -> 'a t -> 'a t -> ('a -> 'a -> 'a -> 'a) -> 'a t
(** [subterm3 g1 g2 g3 f] generates [f x y z]. Shrinks include all three raw
    subterms. *)

val subterm_m : 'a t -> ('a -> 'a t) -> 'a t
(** Like {!subterm} but [f] returns a generator. *)

val subterm_m2 : 'a t -> 'a t -> ('a -> 'a -> 'a t) -> 'a t
(** Like {!subterm2} but [f] returns a generator. *)

val subterm_m3 : 'a t -> 'a t -> 'a t -> ('a -> 'a -> 'a -> 'a t) -> 'a t
(** Like {!subterm3} but [f] returns a generator. *)

(** {2 Conditional} *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Generate values satisfying a predicate. Retries with growing size. After too
    many retries, discards. *)

val discard : 'a t
(** Always discards. *)

val ensure : ('a -> bool) -> 'a t -> 'a t
(** Discard if the generated value doesn't satisfy the predicate. *)

(** {2 Size control} *)

val sized : (int -> 'a t) -> 'a t
(** Construct a generator that depends on the size parameter. *)

val resize : int -> 'a t -> 'a t
(** Override the size parameter. *)

val scale : (int -> int) -> 'a t -> 'a t
(** Adjust the size parameter with a function. *)

val small : 'a t -> 'a t
(** Make a generator smaller by scaling with the golden ratio. *)

(** {2 Shrinking control} *)

val shrink : ('a -> 'a list) -> 'a t -> 'a t
(** Add extra shrinks to a generator. *)

val prune : 'a t -> 'a t
(** Remove all shrinks from a generator. *)

val no_shrink : 'a t -> 'a t
(** Alias for {!prune}. *)

(** {2 Sampling / debugging} *)

val sample : ?size:int -> ?seed:Seed.t -> 'a t -> 'a
(** Generate a single sample. Raises if the generator always discards. *)

val print_tree : ?size:int -> ?seed:Seed.t -> ('a -> string) -> 'a t -> unit
(** Print the shrink tree for debugging. *)

val generate : (int -> Seed.t -> 'a) -> 'a t
(** Create a generator with no shrinks from a size and seed function. *)
