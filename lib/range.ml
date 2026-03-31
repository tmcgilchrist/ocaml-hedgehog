(* Ranges for test data generation, ported from Hedgehog.Internal.Range. *)

type 'a t = { origin : 'a; bounds : int -> 'a * 'a }

let origin r = r.origin
let bounds sz r = r.bounds sz

let lower_bound sz range =
  let x, y = bounds sz range in
  min x y

let upper_bound sz range =
  let x, y = bounds sz range in
  max x y

let singleton x = { origin = x; bounds = (fun _ -> (x, x)) }
let constant_from z x y = { origin = z; bounds = (fun _ -> (x, y)) }
let constant x y = constant_from x x y
let clamp x y n = if x > y then min x (max y n) else min y (max x n)

let scale_linear sz0 z n =
  let sz = max 0 (min 99 sz0) in
  let z' = z in
  let n' = n in
  let rng = n' - z' + compare (n' - z') 0 in
  let diff = rng * sz / 100 in
  z' + diff

let scale_linear_frac sz0 z n =
  let sz = max 0 (min 99 sz0) in
  let diff = (n -. z) *. (float_of_int sz /. 99.0) in
  z +. diff

let scale_exponential_float sz0 z n =
  let sz = clamp 0 99 sz0 in
  let diff =
    (((Float.abs (n -. z) +. 1.0) ** (float_of_int sz /. 99.0)) -. 1.0)
    *. if n -. z >= 0.0 then 1.0 else -1.0
  in
  z +. diff

let scale_exponential sz z n =
  Float.to_int
    (Float.round (scale_exponential_float sz (float_of_int z) (float_of_int n)))

let linear_from z x y =
  {
    origin = z;
    bounds =
      (fun sz ->
        let x_sized = clamp x y (scale_linear sz z x) in
        let y_sized = clamp x y (scale_linear sz z y) in
        (x_sized, y_sized));
  }

let linear x y = linear_from x x y

let linear_frac_from z x y =
  {
    origin = z;
    bounds =
      (fun sz ->
        let x_sized = clamp x y (scale_linear_frac sz z x) in
        let y_sized = clamp x y (scale_linear_frac sz z y) in
        (x_sized, y_sized));
  }

let linear_frac x y = linear_frac_from x x y

let exponential_from z x y =
  {
    origin = z;
    bounds =
      (fun sz ->
        let sized_x = clamp x y (scale_exponential sz z x) in
        let sized_y = clamp x y (scale_exponential sz z y) in
        (sized_x, sized_y));
  }

let exponential x y = exponential_from x x y

let scale_linear_int32 sz0 z n =
  let sz = max 0 (min 99 sz0) in
  let z' = Int32.to_int z in
  let n' = Int32.to_int n in
  let rng = n' - z' + compare (n' - z') 0 in
  let diff = rng * sz / 100 in
  Int32.of_int (z' + diff)

let scale_linear_int64 sz0 z n =
  let sz = max 0 (min 99 sz0) in
  let rng64 = Int64.sub n z in
  let rng = Int64.add rng64 (Int64.of_int (Int64.compare rng64 0L)) in
  let diff = Int64.div (Int64.mul rng (Int64.of_int sz)) 100L in
  Int64.add z diff

let constant_int32 x y = { origin = x; bounds = (fun _ -> (x, y)) }

let linear_int32 x y =
  {
    origin = x;
    bounds =
      (fun sz ->
        let x_sized = clamp x y (scale_linear_int32 sz x x) in
        let y_sized = clamp x y (scale_linear_int32 sz x y) in
        (x_sized, y_sized));
  }

let constant_int64 x y = { origin = x; bounds = (fun _ -> (x, y)) }

let linear_int64 x y =
  {
    origin = x;
    bounds =
      (fun sz ->
        let x_sized = clamp x y (scale_linear_int64 sz x x) in
        let y_sized = clamp x y (scale_linear_int64 sz x y) in
        (x_sized, y_sized));
  }

let exponential_float_from z x y =
  {
    origin = z;
    bounds =
      (fun sz ->
        let sized_x = clamp x y (scale_exponential_float sz z x) in
        let sized_y = clamp x y (scale_exponential_float sz z y) in
        (sized_x, sized_y));
  }

let exponential_float x y = exponential_float_from x x y
