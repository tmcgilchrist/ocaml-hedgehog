(* Pure shrinking strategies ported from Hedgehog.Internal.Shrink. *)

let cons_nub x = function y :: _ as ys when x = y -> ys | ys -> x :: ys

let halves n =
  let rec go n () = if n = 0 then Seq.Nil else Seq.Cons (n, go (n / 2)) in
  go n

let towards destination x =
  if destination = x then Seq.empty
  else if destination = 0 && x = 1 then fun () -> Seq.Cons (0, Seq.empty)
  else
    let diff = (x / 2) - (destination / 2) in
    let h = halves diff in
    let mapped = Seq.map (fun d -> x - d) h in
    fun () ->
      (* cons_nub destination onto the mapped sequence *)
      match mapped () with
      | Seq.Nil -> Seq.Cons (destination, Seq.empty)
      | Seq.Cons (y, rest) ->
          if destination = y then Seq.Cons (y, rest)
          else Seq.Cons (destination, mapped)

let towards_float destination x =
  if destination = x then Seq.empty
  else
    let diff = x -. destination in
    let rec iterate_halve d () =
      let v = x -. d in
      if v = x || Float.is_nan v || Float.is_infinite v then Seq.Nil
      else Seq.Cons (v, iterate_halve (d /. 2.0))
    in
    iterate_halve diff

let halves_int64 n =
  let rec go n () =
    if n = 0L then Seq.Nil else Seq.Cons (n, go (Int64.div n 2L))
  in
  go n

let towards_int64 destination x =
  if destination = x then Seq.empty
  else if destination = 0L && x = 1L then fun () -> Seq.Cons (0L, Seq.empty)
  else
    let diff = Int64.sub (Int64.div x 2L) (Int64.div destination 2L) in
    let h = halves_int64 diff in
    let mapped = Seq.map (fun d -> Int64.sub x d) h in
    fun () ->
      match mapped () with
      | Seq.Nil -> Seq.Cons (destination, Seq.empty)
      | Seq.Cons (y, rest) ->
          if destination = y then Seq.Cons (y, rest)
          else Seq.Cons (destination, mapped)

let removes k xs =
  let rec go xs =
    match xs with
    | [] -> []
    | _ ->
        let xs1 = List.filteri (fun i _ -> i < k) xs in
        let xs2 = List.filteri (fun i _ -> i >= k) xs in
        xs2 :: List.map (fun rest -> xs1 @ rest) (go xs2)
  in
  go xs

let list xs =
  let hs = halves (List.length xs) in
  let rec seq_to_list s =
    match s () with
    | Seq.Nil -> []
    | Seq.Cons (x, rest) -> x :: seq_to_list rest
  in
  List.concat_map (fun k -> removes k xs) (seq_to_list hs)
