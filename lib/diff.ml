type edit = Same of string | Added of string | Removed of string
type t = { edits : edit list }

let compute old_lines new_lines =
  let n = Array.length old_lines in
  let m = Array.length new_lines in
  (* Build LCS length table *)
  let dp = Array.init (n + 1) (fun _ -> Array.make (m + 1) 0) in
  for i = 1 to n do
    for j = 1 to m do
      if old_lines.(i - 1) = new_lines.(j - 1) then
        dp.(i).(j) <- dp.(i - 1).(j - 1) + 1
      else dp.(i).(j) <- max dp.(i - 1).(j) dp.(i).(j - 1)
    done
  done;
  (* Backtrack to produce edits *)
  let edits = ref [] in
  let i = ref n and j = ref m in
  while !i > 0 || !j > 0 do
    if !i > 0 && !j > 0 && old_lines.(!i - 1) = new_lines.(!j - 1) then begin
      edits := Same old_lines.(!i - 1) :: !edits;
      decr i;
      decr j
    end
    else if !j > 0 && (!i = 0 || dp.(!i).(!j - 1) >= dp.(!i - 1).(!j)) then begin
      edits := Added new_lines.(!j - 1) :: !edits;
      decr j
    end
    else begin
      edits := Removed old_lines.(!i - 1) :: !edits;
      decr i
    end
  done;
  { edits = !edits }

let of_strings a b =
  let split s =
    if s = "" then [||] else String.split_on_char '\n' s |> Array.of_list
  in
  compute (split a) (split b)
