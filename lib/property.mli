(** Property testing with OCaml 5 effects for assertions and logging.

    Properties are built by combining generators with effectful test bodies.
    Assertions, logging, and failure are expressed as algebraic effects, handled
    by the property runner. *)

(** {2 Configuration} *)

(** Verbosity of the live progress output. [Normal] reports tests, discards and
    shrinks on stderr as the run proceeds; [Quiet] prints nothing until the run
    finishes. *)
type verbosity = Quiet | Normal

type config = {
  test_limit : int;
  discard_limit : int;
  shrink_limit : int;
  verbosity : verbosity;
}

val default_config : config
(** Defaults: 100 tests, 100 discards, 1000 shrinks. Verbosity is taken from the
    [HEDGEHOG_VERBOSITY] environment variable ([0] for [Quiet], [1] for
    [Normal]) and defaults to [Normal]. *)

(** {2 Results} *)

type diff_values = { left : string; right : string }
(** The two values compared by {!val-diff}, as they were rendered. They are kept
    verbatim rather than diffed at assertion time so that the renderer chosen at
    report time decides how to compare them — see {!section-"diff-rendering"}.
*)

type failure = {
  message : string;
  location : string option;
  diff : diff_values option;
}

type cover = NoCover | Cover

type label_data = {
  label_name : string;
  label_minimum : float;
  label_annotation : cover;
}

type log_entry =
  | Annotation of string
  | Footnote of string
  | Label of label_data

type status =
  | OK
  | Failed of { failure : failure; log : log_entry list }
  | GaveUp

type label_info = { name : string; minimum : float; count : int }

type report = {
  tests : int;
  discards : int;
  shrinks : int;
  status : status;
  coverage : label_info list;
  seed : Seed.t;
  size : int;
}

(** {2 Assertion API (performs effects)} *)

val assert_ : bool -> unit
(** Assert that a condition is true. *)

val ( === ) : 'a -> 'a -> unit
(** Assert structural equality. *)

val diff :
  ('a -> string) -> ('a -> 'b -> bool) -> ('b -> string) -> 'a -> 'b -> unit
(** [diff show_a eq show_b a b] asserts [eq a b], showing a diff on failure. *)

val failure : unit -> 'a
(** Explicitly fail the property. *)

val annotate : string -> unit
(** Log an annotation (shown on failure, before the counterexample). *)

val footnote : string -> unit
(** Log a footnote (shown on failure, after the counterexample). *)

(** {2 Round-trip testing} *)

val tripping :
  ('a -> string) ->
  ('b -> string) ->
  ('a -> 'b) ->
  ('b -> 'a option) ->
  'a ->
  unit
(** [tripping show_a show_b encode decode x] encodes [x] with [encode], then
    decodes with [decode], and asserts that the round-trip produces [Some x]. On
    failure, annotates the original, intermediate, and round-trip values. *)

val eval_result : ('e -> string) -> ('a, 'e) result -> 'a
(** [eval_result show_error r] extracts [Ok x] or fails the property with
    [show_error e] when [r] is [Error e]. *)

(** {2 Coverage / Classification} *)

val cover : float -> string -> bool -> unit
(** [cover minimum name condition] requires at least [minimum]% of tests to
    satisfy [condition] under the given label [name]. Example:
    [cover 30.0 "non-empty" (List.length xs > 0)] *)

val classify : string -> bool -> unit
(** [classify name condition] records the proportion of tests satisfying
    [condition]. Like [cover] with 0% minimum (informational only). *)

val label : string -> unit
(** [label name] labels every test run. Like [cover 0 name true]. *)

val collect : ('a -> string) -> 'a -> unit
(** [collect to_string x] labels using the string representation of [x]. *)

(** {2 Property construction} *)

type property

val property : ?config:config -> (unit -> unit) Gen.t -> property
(** Construct a property from a generator of test closures.

    Example:
    {[
    let prop_reverse =
      property
        Gen.(
          let* xs = list (Range.linear 0 100) (int (Range.linear 0 1000)) in
          return (fun () -> assert_ (List.rev (List.rev xs) = xs)))
    ]} *)

(** {2 Config builders} *)

val with_tests : int -> property -> property
(** Set the number of tests to run. Default [100]. *)

val with_shrinks : int -> property -> property
(** Set the maximum number of shrinks to perform. Default [1000]. *)

val with_discards : int -> property -> property
(** Set the maximum number of discards before giving up. Default [100]. *)

val with_verbose : property -> property
(** Enable live progress reporting to stderr during property checking. This is
    the default unless [HEDGEHOG_VERBOSITY=0] is set. *)

val with_quiet : property -> property
(** Suppress live progress reporting for this property. *)

(** {2:diff-rendering Diff rendering}

    A failing {!val-diff} assertion records the two values as strings; turning
    them into report lines is a separate, replaceable step. The built-in
    renderer runs the line-level LCS diff in {!Hedgehog.Diff}, but any function
    of the same shape can be installed instead — a word-level differ, a
    side-by-side view, or an external library such as [patdiff]. *)

type diff_renderer = color:bool -> left:string -> right:string -> string
(** [render ~color ~left ~right] renders a comparison as report lines.

    [left] is the first value passed to {!val-diff} and [right] the second.
    Lines are returned unindented and newline terminated; the report indents the
    whole block to the position it occupies. When [color] is [true] the renderer
    may emit ANSI escapes. *)

val default_diff_renderer : diff_renderer
(** The built-in renderer: a line-level LCS diff, with removed lines prefixed
    [- ], added lines [+ ], and common lines kept as context. *)

val set_diff_renderer : diff_renderer -> unit
(** Install the renderer used by {!check}, {!format_report}, {!check_group} and
    the [hedgehog-alcotest] integration. Global; call it once at start of day.
    Pass {!default_diff_renderer} to restore the default. *)

val get_diff_renderer : unit -> diff_renderer
(** The currently installed renderer. *)

(** {2 Runner} *)

val check : property -> bool
(** Run a property and return whether it passed. Prints a report on failure. *)

val check_report : property -> report
(** Run a property and return the full report. *)

val format_report :
  ?color:bool -> ?diff_renderer:diff_renderer -> report -> string
(** Format a report as a human-readable string. When [~color:true], ANSI escape
    codes are included for colored output. Defaults to [false]. [~diff_renderer]
    overrides the installed renderer for this report only; see
    {!section-"diff-rendering"}. *)

(** {2 Group runner} *)

type group = { name : string; properties : (string * property) list }

val check_group : group -> bool
(** Run a group of properties sequentially. Prints per-property results and a
    summary line. Returns [true] if all properties passed. *)

val check_sequential : group -> bool
(** Equivalent to {!check_group}. *)

val check_parallel : ?num_domains:int -> group -> bool
(** Run properties in parallel using a domainslib task pool. [num_domains]
    defaults to [Domain.recommended_domain_count () - 1]. Properties sharing
    mutable state may interfere with each other. *)

(** {2 Recheck} *)

val recheck : int -> Seed.t -> property -> report
(** Re-run a property at a specific size and seed for reproducing failures. *)

(** {2 Internal — used by Stm} *)

(** The outcome of running a single test closure under the effect handler. *)
type test_result =
  | TestPassed of log_entry list
      (** The test passed, with any log entries collected. *)
  | TestFailed of failure * log_entry list
      (** The test failed with a failure and any log entries collected. *)

val run_test : (unit -> unit) -> test_result
(** Run a test closure, capturing effects. Returns pass/fail with log. *)
