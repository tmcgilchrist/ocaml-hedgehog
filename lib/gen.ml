(* Generator monad with integrated shrinking.

   A generator takes a size parameter and a seed, and produces a shrink tree
   (or None for discard). *)

type 'a t = int -> Seed.t -> 'a Tree.t option

(* -- Core monad -- *)

let return x _size _seed =
  Some (Tree.singleton x)

let bind m k size seed =
  let (sk, sm) = Seed.split seed in
  match m size sm with
  | None -> None
  | Some tree ->
    (* Recursively bind through the tree, mirroring Haskell's TreeT bind.
       Each child is also bound with k, preserving its shrink subtree. *)
    let rec bind_tree (Tree.Node (x, xs)) =
      match k x size sk with
      | None -> None
      | Some (Tree.Node (y, ys)) ->
        let xs' = Seq.filter_map bind_tree xs in
        Some (Tree.Node (y, Seq.append xs' ys))
    in
    bind_tree tree

let map f m size seed =
  match m size seed with
  | None -> None
  | Some tree -> Some (Tree.map f tree)

let apply mf mx size seed =
  let (sf, sx) = Seed.split seed in
  match mf size sf, mx size sx with
  | Some tf, Some tx -> Some (Tree.mzip tf tx |> Tree.map (fun (f, x) -> f x))
  | _ -> None

let ( let* ) = bind
let ( let+ ) m f = map f m
let ( and+ ) ma mb size seed =
  let (sa, sb) = Seed.split seed in
  match ma size sa, mb size sb with
  | Some ta, Some tb -> Some (Tree.mzip ta tb)
  | _ -> None

(* -- Internal helpers -- *)

let generate f size seed =
  Some (Tree.singleton (f size seed))

(* -- Size control -- *)

let sized f size seed =
  f size size seed

let resize new_size gen _size seed =
  gen new_size seed

let scale f gen size seed =
  let size' = f size in
  if size' < 0 then failwith "Hedgehog.Gen.scale: negative size"
  else gen size' seed

let golden x =
  Float.to_int (Float.round (float_of_int x *. 0.61803398875))

let small gen =
  scale golden gen

(* -- Shrinking control -- *)

let shrink f gen size seed =
  match gen size seed with
  | None -> None
  | Some tree -> Some (Tree.expand (fun x -> List.to_seq (f x)) tree)

let prune gen size seed =
  match gen size seed with
  | None -> None
  | Some tree -> Some (Tree.prune 0 tree)

let no_shrink = prune

(* -- Numeric generators -- *)

let integral range size seed =
  let (lo, hi) = Range.bounds size range in
  let origin = Range.origin range in
  let (n, _) = Seed.next_int lo hi seed in
  if n = origin then
    Some (Tree.singleton n)
  else
    (* Build shrink tree using unfold with binary-search-like shrinking.
       The tree is rooted at n, and each node's children are the shrinks
       towards the origin. cons_child prepends the origin as the first
       child (optimal shrink). *)
    let tree = Tree.unfold Fun.id (fun x -> Shrink.towards origin x) n in
    let tree = Tree.cons_child origin tree in
    Some tree

let int = integral

let float range size seed =
  let (lo, hi) = Range.bounds size range in
  let origin = Range.origin range in
  let (v, _) = Seed.next_float lo hi seed in
  let tree = Tree.singleton v in
  let tree = Tree.expand (fun x ->
    let s = Shrink.towards_float origin x in
    let rec take n s =
      if n = 0 then Seq.empty
      else match s () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons (x, rest) -> fun () -> Seq.Cons (x, take (n-1) rest)
    in
    take 15 s
  ) tree in
  Some tree

let bool _size seed =
  let (n, _) = Seed.next_int 0 1 seed in
  if n = 0 then
    Some (Tree.singleton false)
  else
    Some (Tree.Node (true, fun () -> Seq.Cons (Tree.singleton false, Seq.empty)))

(* -- Character generators -- *)

let char range size seed =
  map Char.chr (integral range) size seed

let digit = char (Range.constant (Char.code '0') (Char.code '9'))
let lower = char (Range.constant (Char.code 'a') (Char.code 'z'))
let upper = char (Range.constant (Char.code 'A') (Char.code 'Z'))

let alpha size seed =
  let (n, seed') = Seed.next_int 0 1 seed in
  if n = 0 then lower size seed'
  else upper size seed'

let alpha_num size seed =
  let (n, seed') = Seed.next_int 0 2 seed in
  if n = 0 then lower size seed'
  else if n = 1 then upper size seed'
  else digit size seed'

let ascii = char (Range.constant 0 127)

(* -- String generator -- *)

let string len_range char_gen size seed =
  let (lo, hi) = Range.bounds size len_range in
  let (n, seed1) = Seed.next_int lo hi seed in
  let len = max 0 n in
  let rec gen_chars remaining seed acc =
    if remaining = 0 then List.rev acc
    else
      let (s1, s2) = Seed.split seed in
      match char_gen size s1 with
      | None -> gen_chars remaining s2 acc
      | Some tree -> gen_chars (remaining - 1) s2 (tree :: acc)
  in
  let char_trees = gen_chars len seed1 [] in
  let result_tree = Tree.interleave char_trees in
  Some (Tree.map (fun chars ->
    String.init (List.length chars) (List.nth chars)
  ) result_tree)

(* -- Choice combinators -- *)

let element xs size seed =
  let n = List.length xs in
  if n = 0 then failwith "Hedgehog.Gen.element: empty list"
  else
    let idx_gen = integral (Range.constant 0 (n - 1)) in
    map (List.nth xs) idx_gen size seed

let choice gens size seed =
  let n = List.length gens in
  if n = 0 then failwith "Hedgehog.Gen.choice: empty list"
  else
    let (idx, seed') = Seed.next_int 0 (n - 1) seed in
    let gen = List.nth gens idx in
    gen size seed'

let frequency weighted_gens size seed =
  match weighted_gens with
  | [] -> failwith "Hedgehog.Gen.frequency: empty list"
  | _ ->
    let total = List.fold_left (fun acc (w, _) -> acc + w) 0 weighted_gens in
    let (n, seed') = Seed.next_int 1 total seed in
    let rec pick remaining = function
      | [] -> failwith "Hedgehog.Gen.frequency: internal error"
      | (w, gen) :: rest ->
        if remaining <= w then gen
        else pick (remaining - w) rest
    in
    let gen = pick n weighted_gens in
    gen size seed'

let recursive f nonrec_gens rec_gens =
  sized (fun size ->
    if size <= 1 then
      f nonrec_gens
    else
      f (nonrec_gens @ List.map small rec_gens))

(* -- Collections -- *)

let list len_range elem_gen size seed =
  let (lo, hi) = Range.bounds size len_range in
  let (n, seed1) = Seed.next_int (max 0 lo) (max 0 hi) seed in
  let len = max 0 n in
  let rec gen_elems remaining seed acc =
    if remaining = 0 then List.rev acc
    else
      let (s1, s2) = Seed.split seed in
      match elem_gen size s1 with
      | None -> gen_elems remaining s2 acc
      | Some tree -> gen_elems (remaining - 1) s2 (tree :: acc)
  in
  let elem_trees = gen_elems len seed1 [] in
  Some (Tree.interleave elem_trees)

let non_empty range elem_gen =
  let* x = elem_gen in
  let* xs = list range elem_gen in
  return (x :: xs)

let shuffle xs =
  let n = List.length xs in
  if n <= 1 then return xs
  else
    let+ keys = list (Range.singleton n) (int (Range.constant 0 (n * n))) in
    if List.length keys <> n then xs
    else
      let paired = List.combine keys xs in
      let sorted = List.stable_sort (fun (a, _) (b, _) -> compare a b) paired in
      List.map snd sorted

let subsequence xs =
  let n = List.length xs in
  if n = 0 then return []
  else
    let+ mask = list (Range.singleton n) bool in
    if List.length mask <> n then []
    else List.map snd (List.filter fst (List.combine mask xs))

let option gen size seed =
  let (n, seed') = Seed.next_int 0 (max 1 size) seed in
  if n = 0 then
    Some (Tree.singleton None)
  else
    map (fun x -> Some x) gen size seed'

let either left_gen right_gen size seed =
  let (n, seed') = Seed.next_int 0 1 seed in
  if n = 0 then map Either.left left_gen size seed'
  else map Either.right right_gen size seed'

let unique cmp range elem_gen =
  let+ xs = list range elem_gen in
  List.sort_uniq cmp xs

let pair ga gb = ( and+ ) ga gb

(* -- Conditional -- *)

let discard _size _seed = None

let ensure p gen size seed =
  match gen size seed with
  | None -> None
  | Some tree ->
    if p (Tree.value tree) then Some tree
    else None

let filter p gen size seed =
  let rec try_gen k size' seed' =
    if k > 100 then None
    else
      let (s1, s2) = Seed.split seed' in
      match gen (size' + 2 * k) s1 with
      | None -> try_gen (k + 1) size' s2
      | Some tree ->
        if p (Tree.value tree) then
          Some (Tree.filter p tree)
        else
          try_gen (k + 1) size' s2
  in
  try_gen 0 size seed

(* -- Freeze -- *)

let freeze gen size seed =
  match gen size seed with
  | None -> None
  | Some tree ->
    Some (Tree.map (fun x -> (x, fun _sz _sd -> Some (Tree.singleton x))) tree)

(* -- Integer width variants -- *)

let int32 range size seed =
  let (lo, hi) = Range.bounds size range in
  let origin = Range.origin range in
  let int_lo = Int32.to_int lo and int_hi = Int32.to_int hi in
  let int_origin = Int32.to_int origin in
  let (n, _) = Seed.next_int int_lo int_hi seed in
  if n = int_origin then Some (Tree.singleton (Int32.of_int n))
  else
    let tree = Tree.unfold Int32.of_int (fun x -> Shrink.towards int_origin x) n in
    let tree = Tree.cons_child (Int32.of_int int_origin) tree in
    Some tree

let int64 range size seed =
  let (lo, hi) = Range.bounds size range in
  let origin = Range.origin range in
  let (n, _) = Seed.next_int64_range lo hi seed in
  if n = origin then Some (Tree.singleton n)
  else
    let tree = Tree.unfold Fun.id (fun x -> Shrink.towards_int64 origin x) n in
    let tree = Tree.cons_child origin tree in
    Some tree

(* -- Subterm combinators -- *)

let subterm gen f size seed =
  match gen size seed with
  | None -> None
  | Some sub_tree ->
    let rec go (Tree.Node (x, xs) as sub) =
      Tree.Node (f x, fun () -> Seq.Cons (sub, Seq.map go xs))
    in
    Some (go sub_tree)

let subterm2 gen1 gen2 f size seed =
  let (s1, s2) = Seed.split seed in
  match gen1 size s1, gen2 size s2 with
  | Some t1, Some t2 ->
    let rec go (Tree.Node (x, xs) as tx) (Tree.Node (y, ys) as ty) =
      Tree.Node (f x y,
        Seq.append
          (fun () -> Seq.Cons (tx, fun () -> Seq.Cons (ty, Seq.empty)))
          (Seq.append
            (Seq.map (fun cx -> go cx ty) xs)
            (Seq.map (fun cy -> go tx cy) ys)))
    in
    Some (go t1 t2)
  | _ -> None

let subterm3 gen1 gen2 gen3 f size seed =
  let (s1, s') = Seed.split seed in
  let (s2, s3) = Seed.split s' in
  match gen1 size s1, gen2 size s2, gen3 size s3 with
  | Some t1, Some t2, Some t3 ->
    let rec go (Tree.Node (x, xs) as tx) (Tree.Node (y, ys) as ty)
               (Tree.Node (z, zs) as tz) =
      Tree.Node (f x y z,
        Seq.append
          (fun () -> Seq.Cons (tx, fun () ->
            Seq.Cons (ty, fun () -> Seq.Cons (tz, Seq.empty))))
          (Seq.append
            (Seq.map (fun cx -> go cx ty tz) xs)
            (Seq.append
              (Seq.map (fun cy -> go tx cy tz) ys)
              (Seq.map (fun cz -> go tx ty cz) zs))))
    in
    Some (go t1 t2 t3)
  | _ -> None

let subterm_m gen f size seed =
  let (sk, sm) = Seed.split seed in
  match gen size sm with
  | None -> None
  | Some sub_tree ->
    let rec go (Tree.Node (x, xs) as sub) =
      match f x size sk with
      | None -> None
      | Some (Tree.Node (y, ys)) ->
        Some (Tree.Node (y,
          fun () -> Seq.Cons (sub, Seq.append (Seq.filter_map go xs) ys)))
    in
    go sub_tree

let subterm_m2 gen1 gen2 f size seed =
  let (s1, s') = Seed.split seed in
  let (s2, sk) = Seed.split s' in
  match gen1 size s1, gen2 size s2 with
  | Some t1, Some t2 ->
    let rec go (Tree.Node (x, xs) as tx) (Tree.Node (y, ys) as ty) =
      match f x y size sk with
      | None -> None
      | Some (Tree.Node (r, rs)) ->
        Some (Tree.Node (r,
          Seq.append
            (fun () -> Seq.Cons (tx, fun () -> Seq.Cons (ty, Seq.empty)))
            (Seq.append
              (Seq.append
                (Seq.filter_map (fun cx -> go cx ty) xs)
                (Seq.filter_map (fun cy -> go tx cy) ys))
              rs)))
    in
    go t1 t2
  | _ -> None

let subterm_m3 gen1 gen2 gen3 f size seed =
  let (s1, s') = Seed.split seed in
  let (s2, s'') = Seed.split s' in
  let (s3, sk) = Seed.split s'' in
  match gen1 size s1, gen2 size s2, gen3 size s3 with
  | Some t1, Some t2, Some t3 ->
    let rec go (Tree.Node (x, xs) as tx) (Tree.Node (y, ys) as ty)
               (Tree.Node (z, zs) as tz) =
      match f x y z size sk with
      | None -> None
      | Some (Tree.Node (r, rs)) ->
        Some (Tree.Node (r,
          Seq.append
            (fun () -> Seq.Cons (tx, fun () ->
              Seq.Cons (ty, fun () -> Seq.Cons (tz, Seq.empty))))
            (Seq.append
              (Seq.append
                (Seq.filter_map (fun cx -> go cx ty tz) xs)
                (Seq.append
                  (Seq.filter_map (fun cy -> go tx cy tz) ys)
                  (Seq.filter_map (fun cz -> go tx ty cz) zs)))
              rs)))
    in
    go t1 t2 t3
  | _ -> None

(* -- Sampling -- *)

let sample ?(size = 30) ?seed gen =
  let seed = match seed with
    | Some s -> s
    | None -> Seed.random ()
  in
  let rec loop n seed =
    if n <= 0 then failwith "Hedgehog.Gen.sample: too many discards"
    else
      let (s1, s2) = Seed.split seed in
      match gen size s1 with
      | None -> loop (n - 1) s2
      | Some tree -> Tree.value tree
  in
  loop 100 seed

let print_tree ?(size = 30) ?seed to_string gen =
  let seed = match seed with
    | Some s -> s
    | None -> Seed.random ()
  in
  match gen size seed with
  | None -> print_string "<discard>\n"
  | Some tree -> print_string (Tree.render to_string tree)
