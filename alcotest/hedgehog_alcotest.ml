let to_alcotest ?(speed_level = `Quick) name prop =
  let run () =
    let open Hedgehog.Property in
    let report = check_report prop in
    match report.status with
    | OK -> ()
    | _ -> Alcotest.fail (format_report report)
  in
  Alcotest.test_case name speed_level run
