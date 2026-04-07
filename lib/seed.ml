(* Splittable pseudorandom number generator.

   Delegates to OCaml's built-in Random.State which uses a splittable LXM
   algorithm (added in OCaml 5.0).

   Random.State.t is mutable, so we copy before each operation to preserve
   the functional (immutable) API that generators expect. *)

type t = Random.State.t

let from x = Random.State.make [| Int64.to_int x |]
let random () = Random.State.make_self_init ()

let split seed =
  let copy = Random.State.copy seed in
  let derived = Random.State.split copy in
  (copy, derived)

let next_int64 seed =
  let copy = Random.State.copy seed in
  let v = Random.State.bits64 copy in
  (v, copy)

let next_int lo hi seed =
  let copy = Random.State.copy seed in
  if lo = hi then (lo, copy)
  else
    let range = hi - lo + 1 in
    if range <= 0 then
      (* Overflow: the full int range is requested. *)
      let v = Random.State.bits64 copy in
      (Int64.to_int v, copy)
    else
      let v = Random.State.full_int copy range in
      (lo + v, copy)

let next_int64_range lo hi seed =
  let copy = Random.State.copy seed in
  if lo = hi then (lo, copy)
  else
    let range = Int64.sub hi lo in
    if range < 0L then
      (* Unsigned overflow: full int64 range requested *)
      let v = Random.State.bits64 copy in
      (v, copy)
    else if range = Int64.max_int then
      (* int64 bound would overflow, use bits64 and mask *)
      let v = Int64.logand (Random.State.bits64 copy) Int64.max_int in
      (Int64.add lo v, copy)
    else
      let v = Random.State.int64 copy (Int64.succ range) in
      (Int64.add lo v, copy)

let next_float lo hi seed =
  let copy = Random.State.copy seed in
  let v = Random.State.float copy (hi -. lo) in
  (lo +. v, copy)
