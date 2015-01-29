(*
  A very restricted interface to the CGI library.

  The `parse_args' function returns the submitted environment as a
  name-value association list.
*)
val parse_args : unit -> (string * string) list
