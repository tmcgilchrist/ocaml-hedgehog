(** Splittable pseudorandom number generator.

    This is a port of "Fast Splittable Pseudorandom Number Generators" by Steele
    et al.

    The paper's algorithm provides decent randomness for most purposes but
    sacrifices cryptographic-quality randomness in favor of speed. *)

type t = {
  value : int64;
  gamma : int64;
}

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
    [\[lo, hi\]]. *)

val next_float : float -> float -> t -> float * t
(** [next_float lo hi seed] generates a random [float] in the range
    [\[lo, hi)]. *)

(** {2 Internal} *)

val golden_gamma : int64
val mix64 : int64 -> int64
val mix64variant13 : int64 -> int64
val mix_gamma : int64 -> int64
