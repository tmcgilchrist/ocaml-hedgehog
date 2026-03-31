(** Ranges describe the bounds of numbers to generate, which may or may not be
    dependent on a size parameter.

    The size is an integer from 0 to 99 inclusive. As the size goes towards 0,
    generated values shrink towards the {!origin}. *)

type 'a t = { origin : 'a; bounds : int -> 'a * 'a }

val origin : 'a t -> 'a
(** Get the origin of a range. When shrinking, values shrink towards the origin.
*)

val bounds : int -> 'a t -> 'a * 'a
(** Get the bounds of a range for a given size. *)

val lower_bound : int -> int t -> int
(** Get the lower bound of a range for the given size. *)

val upper_bound : int -> int t -> int
(** Get the upper bound of a range for the given size. *)

(** {2 Constant} *)

val singleton : 'a -> 'a t
(** A range representing a single constant value. *)

val constant : int -> int -> int t
(** A constant range unaffected by size parameter. Origin is the first argument.
*)

val constant_from : int -> int -> int -> int t
(** [constant_from origin lo hi] creates a constant range from [lo] to [hi] with
    the given [origin]. *)

(** {2 Linear} *)

val linear : int -> int -> int t
(** A range which scales the bounds linearly with the size parameter. Origin is
    the first argument. *)

val linear_from : int -> int -> int -> int t
(** [linear_from origin lo hi] creates a linear range from [lo] to [hi] with the
    given [origin]. *)

val linear_frac : float -> float -> float t
(** Like {!linear}, but for fractional values. *)

val linear_frac_from : float -> float -> float -> float t
(** Like {!linear_from}, but for fractional values. *)

(** {2 Int32} *)

val constant_int32 : int32 -> int32 -> int32 t
(** A constant range for [int32] values, unaffected by size parameter. *)

val linear_int32 : int32 -> int32 -> int32 t
(** A linear range for [int32] values that scales with the size parameter. *)

(** {2 Int64} *)

val constant_int64 : int64 -> int64 -> int64 t
(** A constant range for [int64] values, unaffected by size parameter. *)

val linear_int64 : int64 -> int64 -> int64 t
(** A linear range for [int64] values that scales with the size parameter. *)

(** {2 Exponential} *)

val exponential : int -> int -> int t
(** A range which scales the bounds exponentially with the size parameter. *)

val exponential_from : int -> int -> int -> int t
(** [exponential_from origin lo hi] creates an exponential range. *)

val exponential_float : float -> float -> float t
(** Like {!exponential}, but for floating-point values. *)

val exponential_float_from : float -> float -> float -> float t
(** Like {!exponential_from}, but for floating-point values. *)

(** {2 Internal} *)

val clamp : 'a -> 'a -> 'a -> 'a
val scale_linear : int -> int -> int -> int
val scale_linear_frac : int -> float -> float -> float
val scale_exponential : int -> int -> int -> int
val scale_exponential_float : int -> float -> float -> float
