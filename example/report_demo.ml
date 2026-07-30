(* Demonstrates every shape of the property report, in particular how test,
   discard and shrink counts are rendered.

   Run with:

     dune exec example/report_demo.exe

   Live progress goes to stderr at the default verbosity. To see only the final
   reports, silence it with HEDGEHOG_VERBOSITY=0:

     HEDGEHOG_VERBOSITY=0 dune exec example/report_demo.exe

   Counts vary between runs because each property starts from a random seed. *)

open Hedgehog

let heading s = Printf.printf "\n--- %s ---\n" s

(* Property.check prints nothing when a property passes, so go through
   check_report/format_report to see the successful reports too. *)
let show prop =
  print_string (Property.format_report (Property.check_report prop))

let () =
  heading "passing, nothing discarded";
  show
    Property.(
      property
        Gen.(
          let* n = int (Range.linear 0 100) in
          return (fun () -> assert_ (n >= 0))));

  (* Rejecting one value in five leaves enough successes to reach the test
     limit, so this passes *and* reports its discards. *)
  heading "passing, with discards";
  show
    Property.(
      property
        Gen.(
          let* n = ensure (fun n -> n mod 5 <> 0) (int (Range.linear 0 100)) in
          return (fun () -> assert_ (n mod 5 <> 0))));

  (* Rejecting everything hits the discard limit before the test limit. *)
  heading "gave up: discard limit reached";
  show
    Property.(
      property
        Gen.(
          let* n = ensure (fun _ -> false) (int (Range.linear 0 100)) in
          return (fun () -> assert_ (n >= 0))));

  heading "failing, with shrinks";
  show
    Property.(
      property
        Gen.(
          let* xs = list (Range.linear 0 20) (int (Range.linear 0 100)) in
          return (fun () -> assert_ (List.length xs < 5))));

  (* Both counts at once: the generator discards and the failure shrinks. *)
  heading "failing, with shrinks and discards";
  show
    Property.(
      property
        Gen.(
          let* xs =
            ensure
              (fun xs -> List.length xs <> 3)
              (list (Range.linear 0 20) (int (Range.linear 0 100)))
          in
          return (fun () -> assert_ (List.length xs < 5))));

  heading "group runner: one of each";
  ignore
    Property.(
      check_group
        {
          name = "report demo";
          properties =
            [
              ( "passes cleanly",
                property
                  Gen.(
                    let* n = int (Range.linear 0 10) in
                    return (fun () -> assert_ (n >= 0))) );
              ( "passes with discards",
                property
                  Gen.(
                    let* n =
                      ensure (fun n -> n mod 5 <> 0) (int (Range.linear 0 100))
                    in
                    return (fun () -> assert_ (n mod 5 <> 0))) );
              ( "gives up",
                property
                  Gen.(
                    let* n =
                      ensure (fun _ -> false) (int (Range.linear 0 100))
                    in
                    return (fun () -> assert_ (n >= 0))) );
              ( "fails",
                property
                  Gen.(
                    let* n = int (Range.linear 0 100) in
                    return (fun () -> assert_ (n < 50))) );
            ];
        });

  heading "quiet: same property, no live progress";
  show
    Property.(
      with_quiet
        (property
           Gen.(
             let* n =
               ensure (fun n -> n mod 5 <> 0) (int (Range.linear 0 100))
             in
             return (fun () -> assert_ (n mod 5 <> 0)))))
