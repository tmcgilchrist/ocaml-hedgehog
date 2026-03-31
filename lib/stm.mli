(** Abstract state machine testing.

    Generates random command sequences, executes them against a system under
    test while checking postconditions against a model, and shrinks failures.
    Parallel testing detects concurrency bugs via linearizability checking.

    Inspired by Erlang QuickCheck and qcheck-stm. *)

(** User-provided specification of the system under test. *)
module type Spec = sig
  type cmd
  (** The command type — typically a variant. *)

  type state
  (** Abstract model state. *)

  type sut
  (** System under test (the real implementation). *)

  type result
  (** Result of executing a command — a user-defined type. *)

  val show_cmd : cmd -> string
  (** Pretty-print a command for diagnostic output. *)

  val show_result : result -> string
  (** Pretty-print a result for diagnostic output. *)

  val gen_cmd : state -> cmd Gen.t
  (** Generate a random command given the current model state. *)

  val shrink_cmd : cmd -> cmd Seq.t
  (** Shrink a command to simpler alternatives. *)

  val init_state : state
  (** Initial model state. *)

  val init_sut : unit -> sut
  (** Create a fresh system under test. *)

  val cleanup : sut -> unit
  (** Tear down the system under test. *)

  val next_state : cmd -> state -> state
  (** Advance the model state after a command. *)

  val precond : state -> cmd -> bool
  (** Check whether a command is valid in the given state. *)

  val run : cmd -> sut -> result
  (** Execute a command against the real system. *)

  val postcond : cmd -> state -> result -> bool
  (** Check the result of a command against the model state (before the
      command). *)
end

(** Build a state machine test suite from a specification. *)
module Make (S : Spec) : sig
  val sequential :
    ?config:Property.config -> ?seq_len:int -> unit -> Property.property
  (** Sequential state machine property. Generates command sequences, executes
      them, and checks postconditions at each step. *)

  val parallel :
    ?config:Property.config ->
    ?seq_len:int ->
    ?par_len:int ->
    unit ->
    Property.property
  (** Parallel state machine property. Generates a sequential prefix followed by
      two concurrent branches, then checks linearizability of the observed
      results. *)
end
