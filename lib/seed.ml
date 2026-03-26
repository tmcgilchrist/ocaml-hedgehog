(* Port of "Fast Splittable Pseudorandom Number Generators" by Steele et al.

   Based on Hedgehog.Internal.Seed from haskell-hedgehog.

   OCaml's Int64 arithmetic is modular (wraps on overflow), matching
   Haskell's Word64. *)

type t = {
  value : int64;
  gamma : int64;
}

(* The odd integer closest to 2^64/phi, where phi = (1 + sqrt 5) / 2. *)
let golden_gamma = 0x9e3779b97f4a7c15L

let mix64 x =
  let open Int64 in
  let y = mul (logxor x (shift_right_logical x 33)) 0xff51afd7ed558ccdL in
  let z = mul (logxor y (shift_right_logical y 33)) 0xc4ceb9fe1a85ec53L in
  logxor z (shift_right_logical z 33)

let mix64variant13 x =
  let open Int64 in
  let y = mul (logxor x (shift_right_logical x 30)) 0xbf58476d1ce4e5b9L in
  let z = mul (logxor y (shift_right_logical y 27)) 0x94d049bb133111ebL in
  logxor z (shift_right_logical z 31)

let popcount64 x =
  (* Count the number of set bits in an int64. *)
  let x = ref x in
  let count = ref 0 in
  for _ = 0 to 63 do
    if Int64.logand !x 1L <> 0L then
      incr count;
    x := Int64.shift_right_logical !x 1
  done;
  !count

let mix_gamma x =
  let open Int64 in
  let y = logor (mix64variant13 x) 1L in
  let n = popcount64 (logxor y (shift_right_logical y 1)) in
  if n < 24 then
    logxor y 0xaaaaaaaaaaaaaaaaL
  else
    y

let from x =
  { value = mix64 x;
    gamma = mix_gamma (Int64.add x golden_gamma) }

let random () =
  (* Use stdlib Random to generate seed material without unix dependency. *)
  Random.self_init ();
  let lo = Int64.of_int (Random.bits ()) in
  let hi = Int64.of_int (Random.bits ()) in
  let bits = Int64.logor (Int64.shift_left hi 30) lo in
  from bits

(* Get the next value in the SplitMix sequence. *)
let next seed =
  let v = Int64.add seed.value seed.gamma in
  (v, { seed with value = v })

let split seed =
  let (v0, s1) = next seed in
  let (g0, s2) = next s1 in
  (s2, { value = mix64 v0; gamma = mix_gamma g0 })

let next_int64 seed =
  let (v0, s1) = next seed in
  (mix64 v0, s1)

let next_int lo hi seed =
  if lo = hi then
    (lo, snd (next seed))
  else
    let (v, s) = next_int64 seed in
    (* Map the int64 into [lo, hi] range *)
    let range = hi - lo + 1 in
    if range <= 0 then
      (* Overflow: the full int range is requested.
         Just convert the raw value. *)
      (Int64.to_int v, s)
    else
      let v_int = Int64.to_int v in
      (* Use modulo to get into range, handling negative values *)
      let r = ((v_int mod range) + range) mod range in
      (lo + r, s)

let next_float lo hi seed =
  let (v, s) = next_int64 seed in
  (* Convert to [0, 1) range *)
  let u = Int64.to_float (Int64.shift_right_logical v 11) in
  let unit = u *. (1.0 /. 9007199254740992.0) in (* 2^53 *)
  let result = lo +. (unit *. (hi -. lo)) in
  (result, s)
