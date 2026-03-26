(** Property testing with OCaml 5 effects for assertions and logging.

    Properties are built by combining generators with effectful test bodies.
    Assertions, logging, and failure are expressed as algebraic effects,
    handled by the property runner. *)

(** {2 Configuration} *)

type config = {
  test_limit : int;
  discard_limit : int;
  shrink_limit : int;
}

val default_config : config

(** {2 Results} *)

type failure = {
  message : string;
  location : string option;
}

type log_entry =
  | Annotation of string
  | Footnote of string

type status =
  | OK
  | Failed of { failure : failure; log : log_entry list }
  | GaveUp

type report = {
  tests : int;
  discards : int;
  status : status;
  seed : Seed.t;
  size : int;
}

(** {2 Assertion API (performs effects)} *)

val assert_ : bool -> unit
(** Assert that a condition is true. *)

val ( === ) : 'a -> 'a -> unit
(** Assert structural equality. *)

val diff : ('a -> string) -> ('a -> 'b -> bool) -> ('b -> string) -> 'a -> 'b -> unit
(** [diff show_a eq show_b a b] asserts [eq a b], showing a diff on failure. *)

val failure : unit -> 'a
(** Explicitly fail the property. *)

val annotate : string -> unit
(** Log an annotation (shown on failure, before the counterexample). *)

val footnote : string -> unit
(** Log a footnote (shown on failure, after the counterexample). *)

(** {2 Property construction} *)

type property

val property : ?config:config -> (unit -> unit) Gen.t -> property
(** Construct a property from a generator of test closures.

    Example:
    {[
      let prop_reverse =
        property Gen.(
          let* xs = list (Range.linear 0 100) (int (Range.linear 0 1000)) in
          return (fun () ->
            assert_ (List.rev (List.rev xs) = xs)))
    ]} *)

(** {2 Runner} *)

val check : property -> bool
(** Run a property and return whether it passed. Prints a report on failure. *)

val check_report : property -> report
(** Run a property and return the full report. *)

val recheck : int -> Seed.t -> property -> report
(** Re-run a property at a specific size and seed for reproducing failures. *)
