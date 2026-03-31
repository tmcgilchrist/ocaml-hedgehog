module type Spec = sig
  type cmd
  type state
  type sut
  type result

  val show_cmd : cmd -> string
  val show_result : result -> string
  val gen_cmd : state -> cmd Gen.t
  val shrink_cmd : cmd -> cmd Seq.t
  val init_state : state
  val init_sut : unit -> sut
  val cleanup : sut -> unit
  val next_state : cmd -> state -> state
  val precond : state -> cmd -> bool
  val run : cmd -> sut -> result
  val postcond : cmd -> state -> result -> bool
end

module Make (S : Spec) = struct
  (* Check that a command sequence satisfies all preconditions when replayed
     from init_state. *)
  let preconds_valid cmds =
    let rec go state = function
      | [] -> true
      | cmd :: rest ->
          if S.precond state cmd then go (S.next_state cmd state) rest
          else false
    in
    go S.init_state cmds

  (* Generate a command sequence by threading model state through generation. *)
  let gen_cmds seq_len =
    let open Gen in
    let rec go n state acc =
      if n <= 0 then return (List.rev acc)
      else
        let* cmd = S.gen_cmd state in
        if S.precond state cmd then
          go (n - 1) (S.next_state cmd state) (cmd :: acc)
        else
          (* Precondition failed, try again (don't decrement n) *)
          go n state acc
    in
    go seq_len S.init_state []

  (* Shrink a command list: try removing elements, then shrinking individuals.
     Filter to keep only precondition-valid sequences. *)
  let shrink_cmds cmds =
    let len = List.length cmds in
    (* Remove subsequences of various sizes *)
    let removals =
      let halves n =
        let rec go n =
          if n <= 0 then Seq.empty else fun () -> Seq.Cons (n, go (n / 2))
        in
        go n
      in
      Seq.flat_map
        (fun k ->
          let rec removes k = function
            | [] -> Seq.empty
            | xs ->
                let xs1 = List.filteri (fun i _ -> i < k) xs in
                let xs2 = List.filteri (fun i _ -> i >= k) xs in
                fun () ->
                  Seq.Cons
                    (xs2, Seq.map (fun rest -> xs1 @ rest) (removes k xs2))
          in
          removes k cmds)
        (halves len)
    in
    (* Shrink individual commands *)
    let singles =
      let rec go prefix = function
        | [] -> Seq.empty
        | cmd :: suffix ->
            let shrunk =
              Seq.map
                (fun cmd' -> List.rev_append prefix (cmd' :: suffix))
                (S.shrink_cmd cmd)
            in
            Seq.append shrunk (go (cmd :: prefix) suffix)
      in
      go [] cmds
    in
    Seq.append removals singles |> Seq.filter preconds_valid

  (* Build a shrink tree for a command list. *)
  let cmd_tree cmds = Tree.unfold Fun.id (fun cs -> shrink_cmds cs) cmds

  (* Execute a command sequence on a SUT, checking postconditions. *)
  let run_sequential cmds =
    let sut = S.init_sut () in
    Fun.protect
      ~finally:(fun () -> S.cleanup sut)
      (fun () ->
        let rec go state = function
          | [] -> ()
          | cmd :: rest ->
              let result = S.run cmd sut in
              Property.annotate
                (Printf.sprintf "%s => %s" (S.show_cmd cmd)
                   (S.show_result result));
              if not (S.postcond cmd state result) then Property.failure ()
              else go (S.next_state cmd state) rest
        in
        go S.init_state cmds)

  let sequential ?(config = Property.default_config) ?(seq_len = 20) () =
    let gen size seed =
      match gen_cmds seq_len size seed with
      | None -> None
      | Some cmds_tree ->
          let cmds = Tree.value cmds_tree in
          let tree = cmd_tree cmds in
          Some (Tree.map (fun cs () -> run_sequential cs) tree)
    in
    Property.property ~config gen

  (* -- Parallel testing -- *)

  (* Execute commands and collect a trace. *)
  let exec_trace cmds sut =
    List.map
      (fun cmd ->
        let result = S.run cmd sut in
        (cmd, result))
      cmds

  (* All interleavings of two lists. *)
  let rec all_interleavings xs ys =
    match (xs, ys) with
    | [], _ -> [ ys ]
    | _, [] -> [ xs ]
    | x :: xs', y :: ys' ->
        let left = List.map (fun zs -> x :: zs) (all_interleavings xs' ys) in
        let right = List.map (fun zs -> y :: zs) (all_interleavings xs ys') in
        left @ right

  (* Check if an interleaving is consistent with the model. *)
  let valid_interleaving state trace =
    let rec go state = function
      | [] -> true
      | (cmd, result) :: rest ->
          if S.postcond cmd state result then go (S.next_state cmd state) rest
          else false
    in
    go state trace

  (* Check if two traces are linearizable after a prefix has been executed. *)
  let is_linearizable state trace1 trace2 =
    let interleavings = all_interleavings trace1 trace2 in
    List.exists (valid_interleaving state) interleavings

  let gen_cmd_seq n init_state =
    let open Gen in
    let rec go n state acc =
      if n <= 0 then return (List.rev acc, state)
      else
        let* cmd = S.gen_cmd state in
        if S.precond state cmd then
          go (n - 1) (S.next_state cmd state) (cmd :: acc)
        else go n state acc
    in
    go n init_state []

  (* Generate a parallel test triple: (prefix, branch1, branch2). *)
  let gen_par_cmds seq_len par_len =
    let open Gen in
    let* prefix, state_after_prefix = gen_cmd_seq seq_len S.init_state in
    (* Both branches start from the state after the prefix.
       Generate them independently — preconditions checked against prefix state. *)
    let* branch1, _ = gen_cmd_seq par_len state_after_prefix in
    let* branch2, _ = gen_cmd_seq par_len state_after_prefix in
    return (prefix, branch1, branch2)

  (* Shrink a parallel triple. *)
  let shrink_par_triple (prefix, branch1, branch2) =
    (* Shrink prefix *)
    let sp = Seq.map (fun p -> (p, branch1, branch2)) (shrink_cmds prefix) in
    (* Shrink branch1 *)
    let sb1 = Seq.map (fun b -> (prefix, b, branch2)) (shrink_cmds branch1) in
    (* Shrink branch2 *)
    let sb2 = Seq.map (fun b -> (prefix, branch1, b)) (shrink_cmds branch2) in
    Seq.append sp (Seq.append sb1 sb2)

  let par_tree triple = Tree.unfold Fun.id (fun t -> shrink_par_triple t) triple

  let show_trace trace =
    String.concat "; "
      (List.map
         (fun (cmd, result) ->
           Printf.sprintf "%s => %s" (S.show_cmd cmd) (S.show_result result))
         trace)

  let run_parallel (prefix, branch1, branch2) () =
    let sut = S.init_sut () in
    Fun.protect
      ~finally:(fun () -> S.cleanup sut)
      (fun () ->
        (* Run prefix sequentially *)
        let state_after_prefix =
          List.fold_left
            (fun state cmd ->
              let result = S.run cmd sut in
              if not (S.postcond cmd state result) then begin
                Property.annotate
                  (Printf.sprintf "Prefix failed: %s => %s" (S.show_cmd cmd)
                     (S.show_result result));
                Property.failure ()
              end;
              S.next_state cmd state)
            S.init_state prefix
        in
        (* Run branches concurrently *)
        let trace1 = ref [] in
        let trace2 = ref [] in
        let d1 = Domain.spawn (fun () -> trace1 := exec_trace branch1 sut) in
        let d2 = Domain.spawn (fun () -> trace2 := exec_trace branch2 sut) in
        Domain.join d1;
        Domain.join d2;
        (* Check linearizability *)
        if not (is_linearizable state_after_prefix !trace1 !trace2) then begin
          Property.annotate
            (Printf.sprintf "Prefix: [%s]"
               (String.concat "; " (List.map S.show_cmd prefix)));
          Property.annotate
            (Printf.sprintf "Branch 1: [%s]" (show_trace !trace1));
          Property.annotate
            (Printf.sprintf "Branch 2: [%s]" (show_trace !trace2));
          Property.failure ()
        end)

  let parallel ?(config = Property.default_config) ?(seq_len = 6) ?(par_len = 4)
      () =
    let gen size seed =
      match gen_par_cmds seq_len par_len size seed with
      | None -> None
      | Some triple_tree ->
          let triple = Tree.value triple_tree in
          let tree = par_tree triple in
          Some (Tree.map run_parallel tree)
    in
    Property.property ~config gen
end
