(* Counterexamples that show a diff.

   [Property.assert_] and [===] can only report *that* two values differ.
   [Property.diff] renders the two values and shows a line-level LCS diff of
   them, so the report points at the lines that changed and keeps everything
   else as context. Values printed one field (or one row) per line therefore
   read much better than values printed on a single line.

   The three sections below show:

     1. the same failure reported by [===] and by [diff],
     2. a record round trip, where one field survives and one does not,
     3. a summary report, where the diff lines up rows that did not move.

   Run with:

     dune exec example/diff_demo.exe

   Live progress goes to stderr at the default verbosity. To see only the
   final reports, silence it with HEDGEHOG_VERBOSITY=0:

     HEDGEHOG_VERBOSITY=0 dune exec example/diff_demo.exe

   Counts vary between runs because each property starts from a random seed,
   but shrinking lands on the same minimal counterexample nearly every time. *)

open Hedgehog

let heading s = Printf.printf "\n--- %s ---\n" s
let show prop = ignore (Property.check prop)

(* -------------------------------------------------------------------- *)
(* 1. [===] versus [diff] on the same failure.                          *)
(* -------------------------------------------------------------------- *)

(* Under test: pad a string out to [width] characters. The bug is the [-1],
   which makes every padded string one character short. *)
let pad width s =
  if String.length s >= width then s
  else s ^ String.make (width - String.length s - 1) '.'

let show_padded s = Printf.sprintf "%S (length %d)" s (String.length s)

let gen_padding =
  Gen.(
    let* width = int (Range.linear 4 20) in
    let+ s = string (Range.linear 0 5) (element [ 'a'; 'b' ]) in
    (width, s))

(* -------------------------------------------------------------------- *)
(* 2. A CSV round trip that loses a field to an unescaped separator.     *)
(* -------------------------------------------------------------------- *)

type contact = { id : int; name : string; email : string }

(* One field per line, so a diff can point at the field that changed. *)
let show_contact c =
  String.concat "\n"
    [
      "contact {";
      Printf.sprintf "  id    = %d" c.id;
      Printf.sprintf "  name  = %S" c.name;
      Printf.sprintf "  email = %S" c.email;
      "}";
    ]

(* Under test: comma separated, with no quoting or escaping. A field that
   itself contains a comma silently shifts everything after it. *)
let encode c = Printf.sprintf "%d,%s,%s" c.id c.name c.email

let decode s =
  match String.split_on_char ',' s with
  | id :: name :: email :: _ ->
      Option.map (fun id -> { id; name; email }) (int_of_string_opt id)
  | _ -> None

(* A name may contain a comma ("Doe, Jane"); an address may not. ',' is in
   the name's alphabet, so the generator can produce names that need
   escaping — and shrinking finds the shortest one that does. *)
let gen_name = Gen.(string (Range.linear 1 6) (element [ 'a'; 'b'; ',' ]))

let gen_email =
  Gen.(
    let+ local = string (Range.linear 1 4) (element [ 'a'; 'b' ]) in
    local ^ "@example.com")

let gen_contact =
  Gen.(
    let* id = int (Range.linear 0 999) in
    let* name = gen_name in
    let+ email = gen_email in
    { id; name; email })

let prop_round_trip =
  Property.(
    property
      Gen.(
        let+ c = gen_contact in
        fun () ->
          annotate (Printf.sprintf "encoded: %S" (encode c));
          match decode (encode c) with
          | None -> failure ()
          | Some c' -> diff show_contact ( = ) show_contact c c'))

(* -------------------------------------------------------------------- *)
(* 3. A summary report that forgets to accumulate repeated accounts.     *)
(* -------------------------------------------------------------------- *)

type entry = { account : string; amount : int }

(* Reference implementation: every entry contributes to its account. *)
let totals_reference entries =
  List.fold_left
    (fun acc e ->
      let prev = Option.value ~default:0 (List.assoc_opt e.account acc) in
      (e.account, prev + e.amount) :: List.remove_assoc e.account acc)
    [] entries
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

(* Under test: [Hashtbl.replace] keeps the last amount seen for an account
   instead of adding to the running total. *)
let totals_under_test entries =
  let tbl = Hashtbl.create 8 in
  List.iter (fun e -> Hashtbl.replace tbl e.account e.amount) entries;
  Hashtbl.fold (fun account amount acc -> (account, amount) :: acc) tbl []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

(* One row per line, plus a total, so the diff aligns the rows that did not
   move and marks only the ones that did. *)
let show_report totals =
  let row (account, amount) = Printf.sprintf "  %-6s %4d" account amount in
  let total = List.fold_left (fun acc (_, n) -> acc + n) 0 totals in
  String.concat "\n"
    (("  account amount" :: List.map row totals)
    @ [ "  ------- ------"; row ("TOTAL", total) ])

let show_entries entries =
  "["
  ^ String.concat "; "
      (List.map (fun e -> Printf.sprintf "%s=%d" e.account e.amount) entries)
  ^ "]"

let gen_entries =
  Gen.(
    list (Range.linear 0 6)
      (let* account = element [ "ada"; "bob"; "cyd" ] in
       let+ amount = int (Range.linear 0 50) in
       { account; amount }))

let () =
  (* [===] knows the values differ but cannot show them: the report says
     "not equal" and stops there. *)
  heading "1a. === : no counterexample detail";
  show
    Property.(
      property
        Gen.(
          let+ width, s = gen_padding in
          fun () -> String.length (pad width s) === max width (String.length s)));

  (* [diff] takes a printer for each side, so the same failure arrives with
     the values attached. '-' lines are the first value passed to [diff],
     '+' lines the second — here, expected then actual. *)
  heading "1b. diff : the same failure, with the values";
  show
    Property.(
      property
        Gen.(
          let+ width, s = gen_padding in
          fun () ->
            let expected = s ^ String.make (max 0 (width - String.length s)) '.'
            and actual = pad width s in
            diff show_padded ( = ) show_padded expected actual));

  (* Values rendered over several lines are where the diff earns its keep:
     unchanged fields become context and only the broken one is marked.
     Shrinking reduces the contact to the shortest one that still needs
     escaping, so the report is about the comma and nothing else. *)
  heading "2. round trip: which field did we lose?";
  show prop_round_trip;

  (* Comparing an implementation against a reference is the natural home for
     [diff]: the report shows the rows that agree as context and the rows
     that disagree as edits.

     The counterexample is always two entries for one account, but which
     account varies between runs. Shrinking one entry's name to "ada" on its
     own makes the accounts differ and the property pass, and shrinks are
     tried one at a time, so the pair is already minimal whichever name it
     landed on. *)
  heading "3. report: which rows are wrong?";
  show
    Property.(
      property
        Gen.(
          let+ entries = gen_entries in
          fun () ->
            annotate ("entries: " ^ show_entries entries);
            footnote "'-' is the reference report, '+' is ours";
            diff show_report ( = ) show_report (totals_reference entries)
              (totals_under_test entries)))
