(** Line-level diff algorithm based on longest common subsequence. *)

type edit = Same of string | Added of string | Removed of string
(** A single edit operation in the diff. *)

type t = { edits : edit list }
(** A diff is a list of edits. *)

val compute : string array -> string array -> t
(** [compute old_lines new_lines] computes a line-level diff between
    two arrays of lines using the LCS algorithm. *)

val of_strings : string -> string -> t
(** [of_strings a b] splits both strings on newlines and computes the diff. *)
