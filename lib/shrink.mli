(** Pure shrinking strategies.

    These functions produce lists of shrunk alternatives from a value.
    They are used by generators to build shrink trees. *)

val towards : int -> int -> int Seq.t
(** [towards destination x] shrinks [x] by edging towards [destination].

    {[
      towards 0 100 = [0; 50; 75; 88; 94; 97; 99]
      towards 500 1000 = [500; 750; 875; ...]
    ]}

    The destination is always tried first, as that is the optimal shrink. *)

val towards_float : float -> float -> float Seq.t
(** [towards_float destination x] shrinks a float towards [destination]. *)

val towards_int64 : int64 -> int64 -> int64 Seq.t
(** [towards_int64 destination x] shrinks an [int64] towards [destination],
    using binary-search halving. Same algorithm as {!towards} but with
    [Int64] arithmetic. *)

val halves_int64 : int64 -> int64 Seq.t
(** Progressive halving of an [int64]. *)

val halves : int -> int Seq.t
(** Progressive halving of an integral.

    {[
      halves 15 = [15; 7; 3; 1]
      halves 100 = [100; 50; 25; 12; 6; 3; 1]
    ]} *)

val list : 'a list -> 'a list list
(** Shrink a list by edging towards the empty list.

    {[
      list [1;2;3] = [[];  [2;3]; [1;3]; [1;2]]
    ]} *)

val removes : int -> 'a list -> 'a list list
(** [removes k xs] produces all permutations of removing [k] elements
    from [xs].

    {[
      removes 2 ['a';'b';'c';'d';'e';'f'] = [['c';'d';'e';'f']; ['a';'b';'e';'f']; ['a';'b';'c';'d']]
    ]} *)

val cons_nub : 'a -> 'a list -> 'a list
(** [cons_nub x ys] prepends [x] to [ys] unless [x] equals the head
    of [ys]. *)
