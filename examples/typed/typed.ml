(*pp /usr/local/ocaml/bin/camlp4of -I ../../pp desugar.cmo desugar_typed.cmo desugar_contexts.cmo lexer.cmo parser.cmo syntax.cmo  *)

<<

(*
  Formlet desugaring using a translation to typed XHTML.
*)
open Formlets_typed

(* This is allowed *)
let test : (_, int) formlet =
  formlet typed
    <p><em>foo</em></p>
  yields
    2

(* This is not allowed (uncomment it and see) *)
(*
let test : (_, int) formlet =
  formlet typed
    <p><p>foo</p></p>
  yields
    2
*)
  
>>

