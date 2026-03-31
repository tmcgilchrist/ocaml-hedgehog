(* Rose tree with lazy children (Seq.t) for representing generated values
   and their shrinks. *)

type 'a t = Node of 'a * 'a t Seq.t

let value (Node (x, _)) = x
let children (Node (_, xs)) = xs
let singleton x = Node (x, Seq.empty)
let return = singleton
let rec map f (Node (x, xs)) = Node (f x, Seq.map (map f) xs)

let rec bind (Node (x, xs)) k =
  match k x with
  | Node (y, ys) ->
      let xs' = Seq.map (fun m -> bind m k) xs in
      Node (y, Seq.append xs' ys)

let join xss = bind xss Fun.id

let rec fold f g (Node (x, xs)) = f x (fold_forest f g xs)
and fold_forest f g xs = g (Seq.map (fold f g) xs)

let rec unfold f g x = Node (f x, unfold_forest f g x)
and unfold_forest f g x = Seq.map (unfold f g) (g x)

let rec expand f (Node (x, xs)) =
  let ys = Seq.map (expand f) xs in
  let zs = unfold_forest Fun.id f x in
  Node (x, Seq.append ys zs)

let rec prune n (Node (x, xs)) =
  if n <= 0 then Node (x, Seq.empty) else Node (x, Seq.map (prune (n - 1)) xs)

let rec filter p (Node (x, xs)) = Node (x, filter_forest p xs)

and filter_forest p xs =
  xs |> Seq.filter (fun t -> p (value t)) |> Seq.map (filter p)

let rec map_option p (Node (x, xs)) =
  match p x with
  | None -> (
      (* Root doesn't match; try to find a passing child to promote. *)
      let passing = Seq.filter_map (map_option p) xs in
      match passing () with
      | Seq.Nil -> None
      | Seq.Cons (Node (y, ys0), rest) -> Some (Node (y, Seq.append ys0 rest)))
  | Some y -> Some (Node (y, Seq.filter_map (map_option p) xs))

let mzip (Node (a, ls)) t0 =
  let rec go l0 r0 =
    let (Node (a', ls')) = l0 in
    let (Node (b', rs')) = r0 in
    let left_shrinks = Seq.map (fun l1 -> go l1 r0) ls' in
    let right_shrinks = Seq.map (fun r1 -> go l0 r1) rs' in
    Node ((a', b'), Seq.append left_shrinks right_shrinks)
  in
  go (Node (a, ls)) t0

let cons_child a (Node (x, xs)) = Node (x, fun () -> Seq.Cons (singleton a, xs))

(* -- List interleaving for optimal list shrinking -- *)

(* All ways to split a list into (prefix, element, suffix). *)
let splits xs =
  let rec go front = function
    | [] -> []
    | x :: rest -> (List.rev front, x, rest) :: go (x :: front) rest
  in
  go [] xs

(* removes k xs: all ways to remove chunks of size k from xs.
   Mirrors Hedgehog's Tree.removes. *)
let removes k xs =
  let rec go = function
    | [] -> []
    | xs ->
        let xs1, xs2 =
          ( List.filteri (fun i _ -> i < k) xs,
            List.filteri (fun i _ -> i >= k) xs )
        in
        xs2 :: List.map (fun rest -> xs1 @ rest) (go xs2)
  in
  go xs

(* Progressive halvings: [n, n/2, n/4, ..., 1] for n > 0. *)
let halves n =
  let rec go n = if n <= 0 then [] else n :: go (n / 2) in
  go n

(* Node type used internally for interleaving. *)
type 'a node = { nvalue : 'a; nchildren : 'a t list }

(* Convert tree to internal node, forcing only direct children. *)
let to_node (Node (x, xs)) = { nvalue = x; nchildren = List.of_seq xs }

(* Lazy concat of list-producing functions over a list.
   Each function application is deferred. *)
let rec lazy_concat_map f xs () =
  match xs with
  | [] -> Seq.Nil
  | x :: rest -> lazy_append_seq (List.to_seq (f x)) (lazy_concat_map f rest) ()

and lazy_append_seq s1 s2 () =
  match s1 () with
  | Seq.Nil -> s2 ()
  | Seq.Cons (x, rest) -> Seq.Cons (x, lazy_append_seq rest s2)

(* dropSome: try removing chunks of elements — returns lazy Seq *)
let rec drop_some (nodes : 'a node list) : 'a list t Seq.t =
  let ns = halves (List.length nodes) in
  lazy_concat_map
    (fun n ->
      List.map (fun nodes' -> interleave_nodes nodes') (removes n nodes))
    ns

(* shrinkOne: try shrinking one element at a time — returns lazy Seq *)
and shrink_one (nodes : 'a node list) : 'a list t Seq.t =
  lazy_concat_map
    (fun (xs, y, zs) ->
      List.map
        (fun child ->
          let child_node = to_node child in
          interleave_nodes (xs @ [ child_node ] @ zs))
        y.nchildren)
    (splits nodes)

and interleave_nodes (nodes : 'a node list) : 'a list t =
  let values = List.map (fun n -> n.nvalue) nodes in
  let shrinks = Seq.append (drop_some nodes) (shrink_one nodes) in
  Node (values, shrinks)

let interleave (trees : 'a t list) : 'a list t =
  let nodes = List.map to_node trees in
  interleave_nodes nodes

let rec depth (Node (_, xs)) =
  match xs () with
  | Seq.Nil -> 1
  | _ ->
      let max_child = Seq.fold_left (fun acc t -> max acc (depth t)) 0 xs in
      1 + max_child

(* -- Rendering -- *)

let render to_string tree =
  let buf = Buffer.create 256 in
  let rec render_lines (Node (x, xs)) =
    let s = to_string x in
    let child_list = List.of_seq xs in
    s :: render_forest child_list
  and render_forest = function
    | [] -> []
    | [ x ] ->
        let lines = render_lines x in
        shift " └╼" "   " lines
    | x :: rest ->
        let lines = render_lines x in
        let rest_lines = render_forest rest in
        shift " ├╼" " │ " lines @ rest_lines
  and shift first_prefix other_prefix = function
    | [] -> []
    | line :: rest ->
        (first_prefix ^ line) :: List.map (fun l -> other_prefix ^ l) rest
  in
  let lines = render_lines tree in
  List.iter
    (fun line ->
      Buffer.add_string buf line;
      Buffer.add_char buf '\n')
    lines;
  Buffer.contents buf
