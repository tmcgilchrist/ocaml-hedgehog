(* Property testing with OCaml 5 algebraic effects.

   Effects replace the TestT monad transformer layer from Haskell Hedgehog.
   Assertions, logging, and failure are expressed as effects, handled by
   the property runner. *)

(* -- Configuration -- *)

type config = {
  test_limit : int;
  discard_limit : int;
  shrink_limit : int;
}

let default_config = {
  test_limit = 100;
  discard_limit = 100;
  shrink_limit = 1000;
}

(* -- Result types -- *)

type failure = {
  message : string;
  location : string option;
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

type label_info = {
  name : string;
  minimum : float;
  count : int;
}

type report = {
  tests : int;
  discards : int;
  status : status;
  coverage : label_info list;
  seed : Seed.t;
  size : int;
}

(* -- Effects -- *)

type _ Effect.t +=
  | Fail : failure -> 'a Effect.t
  | WriteLog : log_entry -> unit Effect.t

(* -- Assertion API -- *)

let assert_ b =
  if not b then
    Effect.perform (Fail { message = "Assertion failed"; location = None })

let ( === ) a b =
  if a <> b then
    Effect.perform (Fail { message = "not equal"; location = None })

let diff show_a eq show_b a b =
  if not (eq a b) then
    let msg = Printf.sprintf "expected: %s\n got: %s" (show_a a) (show_b b) in
    Effect.perform (Fail { message = msg; location = None })

let failure () =
  Effect.perform (Fail { message = "explicit failure"; location = None })

let annotate s =
  Effect.perform (WriteLog (Annotation s))

let footnote s =
  Effect.perform (WriteLog (Footnote s))

let cover minimum name covered =
  let ann = if covered then Cover else NoCover in
  Effect.perform (WriteLog (Label { label_name = name;
                                     label_minimum = minimum;
                                     label_annotation = ann }))

let classify name covered = cover 0.0 name covered
let label name = cover 0.0 name true
let collect to_string x = cover 0.0 (to_string x) true

(* -- Property type -- *)

type property = {
  config : config;
  gen : (unit -> unit) Gen.t;
}

let property ?(config = default_config) gen =
  { config; gen }

let map_config f prop =
  { prop with config = f prop.config }

let with_tests n =
  map_config (fun c -> { c with test_limit = n })

let with_shrinks n =
  map_config (fun c -> { c with shrink_limit = n })

let with_discards n =
  map_config (fun c -> { c with discard_limit = n })

(* -- Effect handler: run a test closure and capture result -- *)

type test_result =
  | TestPassed of log_entry list
  | TestFailed of failure * log_entry list

let run_test (f : unit -> unit) : test_result =
  let log = ref [] in
  match
    Effect.Deep.match_with f ()
      { retc = (fun () -> TestPassed (List.rev !log));
        exnc = (fun e ->
          TestFailed ({ message = Printexc.to_string e; location = None },
                      List.rev !log));
        effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Fail failure ->
            Some (fun (_k : (a, _) Effect.Deep.continuation) ->
              TestFailed (failure, List.rev !log))
          | WriteLog entry ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              log := entry :: !log;
              Effect.Deep.continue k ())
          | _ -> None }
  with
  | result -> result

(* -- Shrinking: walk the tree to find minimal counterexample -- *)

let take_smallest shrink_limit tree =
  let shrinks_performed = ref 0 in
  let rec go (Tree.Node (test_fn, children)) =
    match run_test test_fn with
    | TestPassed _ ->
      (* This node passes; it's not the counterexample we want *)
      None
    | TestFailed (fail, log) ->
      (* This node fails; try to find a smaller failing child *)
      let result = ref (fail, log) in
      let found_smaller = ref false in
      let try_children children =
        let rec try_seq s =
          if !shrinks_performed >= shrink_limit then ()
          else
            match s () with
            | Seq.Nil -> ()
            | Seq.Cons (child, rest) ->
              incr shrinks_performed;
              (match go child with
               | Some (f, l) ->
                 result := (f, l);
                 found_smaller := true
                 (* Don't continue after finding a failing child —
                    commit to this path *)
               | None ->
                 try_seq rest)
        in
        try_seq children
      in
      try_children children;
      ignore !found_smaller;
      Some !result
  in
  go tree

(* -- Coverage accumulation -- *)

module LabelMap = Map.Make(String)

type label_count = {
  lc_minimum : float;
  lc_count : int;
}

let journal_coverage (log : log_entry list) : label_count LabelMap.t =
  List.fold_left (fun acc -> function
    | Label { label_name; label_minimum; label_annotation } ->
      let count = match label_annotation with Cover -> 1 | NoCover -> 0 in
      LabelMap.update label_name (function
        | None -> Some { lc_minimum = label_minimum; lc_count = count }
        | Some lc -> Some { lc_count = lc.lc_count + count;
                            lc_minimum = Float.max lc.lc_minimum label_minimum }
      ) acc
    | _ -> acc
  ) LabelMap.empty log

let merge_coverage a b =
  LabelMap.union (fun _name x y ->
    Some { lc_minimum = Float.max x.lc_minimum y.lc_minimum;
           lc_count = x.lc_count + y.lc_count }
  ) a b

let coverage_failures tests (cov : label_count LabelMap.t) =
  LabelMap.fold (fun name lc acc ->
    let pct = float_of_int lc.lc_count /. float_of_int tests *. 100.0 in
    if pct < lc.lc_minimum then (name, lc.lc_minimum, pct) :: acc else acc
  ) cov []

let coverage_to_list (cov : label_count LabelMap.t) : label_info list =
  LabelMap.fold (fun name lc acc ->
    { name; minimum = lc.lc_minimum; count = lc.lc_count } :: acc
  ) cov []

(* -- Runner -- *)

let check_report prop =
  let config = prop.config in
  let initial_seed = Seed.random () in
  let rec loop tests discards size seed cov =
    if tests >= config.test_limit then
      let coverage = coverage_to_list cov in
      let failures = coverage_failures tests cov in
      if failures <> [] then
        let msg = String.concat "\n" (List.map (fun (name, min, actual) ->
          Printf.sprintf "  %s: %.1f%% (minimum: %.1f%%)" name actual min
        ) failures) in
        { tests; discards;
          status = Failed { failure = { message = "Insufficient coverage after "
            ^ string_of_int tests ^ " tests.\n" ^ msg; location = None };
            log = [] };
          coverage; seed = initial_seed; size }
      else
        { tests; discards; status = OK; coverage; seed = initial_seed; size }
    else if discards >= config.discard_limit then
      { tests; discards; status = GaveUp;
        coverage = coverage_to_list cov; seed = initial_seed; size }
    else
      let (s1, s2) = Seed.split seed in
      let current_size = size mod 100 in
      match prop.gen current_size s1 with
      | None ->
        loop tests (discards + 1) (size + 1) s2 cov
      | Some tree ->
        let test_fn = Tree.value tree in
        (match run_test test_fn with
         | TestPassed log ->
           let test_cov = journal_coverage log in
           loop (tests + 1) discards (size + 1) s2 (merge_coverage cov test_cov)
         | TestFailed (fail, log) ->
           let (final_fail, final_log) =
             match take_smallest config.shrink_limit tree with
             | Some (f, l) -> (f, l)
             | None -> (fail, log)
           in
           { tests = tests + 1;
             discards;
             status = Failed { failure = final_fail; log = final_log };
             coverage = coverage_to_list cov;
             seed = initial_seed;
             size = current_size })
  in
  loop 0 0 0 initial_seed LabelMap.empty

let format_coverage buf tests coverage =
  if coverage <> [] then
    List.iter (fun { name; minimum; count } ->
      let pct = float_of_int count /. float_of_int tests *. 100.0 in
      let mark = if pct >= minimum then "✓" else "✗" in
      Buffer.add_string buf
        (Printf.sprintf "    %s %s  %.1f%% (%d/%d, minimum: %.1f%%)\n"
           mark name pct count tests minimum)
    ) coverage

let format_report report =
  let buf = Buffer.create 256 in
  (match report.status with
   | OK ->
     Buffer.add_string buf (Printf.sprintf "+++ OK, passed %d tests.\n" report.tests);
     format_coverage buf report.tests report.coverage
   | GaveUp ->
     Buffer.add_string buf
       (Printf.sprintf "*** Gave up after %d discards, passed %d tests.\n"
          report.discards report.tests)
   | Failed { failure; log } ->
     Buffer.add_string buf
       (Printf.sprintf "*** Failed! Falsifiable (after %d tests):\n" report.tests);
     List.iter (fun entry ->
       match entry with
       | Annotation s -> Buffer.add_string buf (Printf.sprintf "  %s\n" s)
       | Footnote s -> Buffer.add_string buf (Printf.sprintf "  [footnote] %s\n" s)
       | Label _ -> ()
     ) log;
     Buffer.add_string buf (Printf.sprintf "  %s\n" failure.message);
     format_coverage buf report.tests report.coverage);
  Buffer.contents buf

let check prop =
  let report = check_report prop in
  (match report.status with
   | OK -> ()
   | _ -> print_string (format_report report));
  match report.status with
  | OK -> true
  | _ -> false

(* -- Group runner -- *)

type group = {
  name : string;
  properties : (string * property) list;
}

let print_property_result name report =
  match report.status with
  | OK ->
    Printf.printf "  ✓ %s passed %d tests.\n" name report.tests
  | GaveUp ->
    Printf.printf "  ⚐ %s gave up after %d discards, only %d tests.\n"
      name report.discards report.tests
  | Failed { failure; log } ->
    Printf.printf "  ✗ %s failed after %d tests.\n" name report.tests;
    List.iter (fun entry ->
      match entry with
      | Annotation s -> Printf.printf "      %s\n" s
      | Footnote s -> Printf.printf "      [footnote] %s\n" s
      | Label _ -> ()
    ) log;
    Printf.printf "      %s\n" failure.message

let print_group_footer ok failed gave_up =
  let bar = "━━━" in
  let parts = ref [] in
  if failed > 0 then
    parts := Printf.sprintf "%d failed" failed :: !parts;
  if gave_up > 0 then
    parts := Printf.sprintf "%d gave up" gave_up :: !parts;
  if ok > 0 then
    parts := Printf.sprintf "%d succeeded" ok :: !parts;
  let summary = String.concat ", " (List.rev !parts) in
  Printf.printf "%s %s. %s\n" bar summary bar;
  failed = 0 && gave_up = 0

let check_group group =
  let bar = "━━━" in
  Printf.printf "%s %s %s\n" bar group.name bar;
  let ok = ref 0 in
  let failed = ref 0 in
  let gave_up = ref 0 in
  List.iter (fun (name, prop) ->
    let report = check_report prop in
    print_property_result name report;
    match report.status with
    | OK -> incr ok
    | GaveUp -> incr gave_up
    | Failed _ -> incr failed
  ) group.properties;
  print_group_footer !ok !failed !gave_up

let check_sequential = check_group

let check_parallel ?(num_domains = max 1 (Domain.recommended_domain_count () - 1)) group =
  let bar = "━━━" in
  Printf.printf "%s %s %s\n" bar group.name bar;
  let pool = Domainslib.Task.setup_pool ~num_domains () in
  let result = Domainslib.Task.run pool (fun () ->
    let promises = List.map (fun (name, prop) ->
      (name, Domainslib.Task.async pool (fun () -> check_report prop)))
      group.properties
    in
    let results = List.map (fun (name, p) ->
      (name, Domainslib.Task.await pool p)) promises
    in
    let ok = ref 0 and failed = ref 0 and gave_up = ref 0 in
    List.iter (fun (name, report) ->
      print_property_result name report;
      match report.status with
      | OK -> incr ok
      | GaveUp -> incr gave_up
      | Failed _ -> incr failed
    ) results;
    print_group_footer !ok !failed !gave_up
  ) in
  Domainslib.Task.teardown_pool pool;
  result

let recheck size seed prop =
  match prop.gen size seed with
  | None ->
    { tests = 1; discards = 1; status = GaveUp; coverage = []; seed; size }
  | Some tree ->
    let test_fn = Tree.value tree in
    match run_test test_fn with
    | TestPassed log ->
      let cov = journal_coverage log in
      { tests = 1; discards = 0; status = OK;
        coverage = coverage_to_list cov; seed; size }
    | TestFailed (fail, log) ->
      let (final_fail, final_log) =
        match take_smallest prop.config.shrink_limit tree with
        | Some (f, l) -> (f, l)
        | None -> (fail, log)
      in
      { tests = 1;
        discards = 0;
        status = Failed { failure = final_fail; log = final_log };
        coverage = [];
        seed;
        size }
