(** Splittable pseudorandom number generator.

    Wraps OCaml's built-in [Random.State] which uses a splittable LXM
    algorithm. *)

type t = Random.State.t

val from : int64 -> t
(** Create a seed from an [int64] value. *)

val random : unit -> t
(** Create a random seed using the system clock. *)

val split : t -> t * t
(** Split a seed into two independent seeds. *)

val next_int64 : t -> int64 * t
(** Generate a random [int64]. *)

val next_int : int -> int -> t -> int * t
(** [next_int lo hi seed] generates a random [int] in the inclusive range
    [[lo, hi]]. *)

val next_int64_range : int64 -> int64 -> t -> int64 * t
(** [next_int64_range lo hi seed] generates a random [int64] in the inclusive
    range [[lo, hi]]. *)

val next_float : float -> float -> t -> float * t
(** [next_float lo hi seed] generates a random [float] in the range [\[lo, hi)].
*)
