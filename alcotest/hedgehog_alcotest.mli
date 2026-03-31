(** Alcotest integration for Hedgehog properties. *)

val to_alcotest :
  ?speed_level:Alcotest.speed_level ->
  string ->
  Hedgehog.Property.property ->
  unit Alcotest.test_case
(** [to_alcotest ~speed_level name prop] wraps a Hedgehog property as an
    Alcotest test case. If the property fails or gives up, the test case calls
    [Alcotest.fail] with the formatted report. *)
