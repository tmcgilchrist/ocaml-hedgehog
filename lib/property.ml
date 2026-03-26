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

(* -- Property type -- *)

type property = {
  config : config;
  gen : (unit -> unit) Gen.t;
}

let property ?(config = default_config) gen =
  { config; gen }

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

(* -- Runner -- *)

let check_report prop =
  let config = prop.config in
  let initial_seed = Seed.random () in
  let rec loop tests discards size seed =
    if tests >= config.test_limit then
      { tests; discards; status = OK; seed = initial_seed; size }
    else if discards >= config.discard_limit then
      { tests; discards; status = GaveUp; seed = initial_seed; size }
    else
      let (s1, s2) = Seed.split seed in
      let current_size = size mod 100 in
      match prop.gen current_size s1 with
      | None ->
        (* Discard *)
        loop tests (discards + 1) (size + 1) s2
      | Some tree ->
        (* Got a test closure tree *)
        let test_fn = Tree.value tree in
        (match run_test test_fn with
         | TestPassed _ ->
           loop (tests + 1) discards (size + 1) s2
         | TestFailed (fail, log) ->
           (* Found a failure! Now shrink. *)
           let (final_fail, final_log) =
             match take_smallest config.shrink_limit tree with
             | Some (f, l) -> (f, l)
             | None -> (fail, log)
           in
           { tests = tests + 1;
             discards;
             status = Failed { failure = final_fail; log = final_log };
             seed = initial_seed;
             size = current_size })
  in
  loop 0 0 0 initial_seed

let format_report report =
  let buf = Buffer.create 256 in
  (match report.status with
   | OK ->
     Buffer.add_string buf (Printf.sprintf "+++ OK, passed %d tests.\n" report.tests)
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
     ) log;
     Buffer.add_string buf (Printf.sprintf "  %s\n" failure.message));
  Buffer.contents buf

let check prop =
  let report = check_report prop in
  (match report.status with
   | OK -> ()
   | _ -> print_string (format_report report));
  match report.status with
  | OK -> true
  | _ -> false

let recheck size seed prop =
  match prop.gen size seed with
  | None ->
    { tests = 1; discards = 1; status = GaveUp; seed; size }
  | Some tree ->
    let test_fn = Tree.value tree in
    match run_test test_fn with
    | TestPassed _ ->
      { tests = 1; discards = 0; status = OK; seed; size }
    | TestFailed (fail, log) ->
      let (final_fail, final_log) =
        match take_smallest prop.config.shrink_limit tree with
        | Some (f, l) -> (f, l)
        | None -> (fail, log)
      in
      { tests = 1;
        discards = 0;
        status = Failed { failure = final_fail; log = final_log };
        seed;
        size }
