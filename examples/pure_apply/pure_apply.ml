(*pp /usr/local/ocaml/bin/camlp4of -I ../../pp desugar.cmo desugar_typed.cmo desugar_contexts.cmo lexer.cmo parser.cmo syntax.cmo  *)

<<

(*
  A demonstration that the formlet syntax is sufficient for defining
  the primitive `pure' and `apply' combinators.
*)

(*
  The `pure' and `apply' functions using regular formlets.
*)
open Formlets

let my_pure : 'a -> 'a formlet =
  fun v -> formlet <#/> yields  v

let my_apply : ('a -> 'b) formlet -> 'a formlet -> 'b formlet =
  fun f p -> formlet <#>{f => g}{p => q}</#> yields g q

(*
  The `pure' and `apply' functions using typed XML.
*)
open Formlets_typed

let my_pure : 'a -> (_, 'a) formlet =
  fun v -> formlet typed <#/> yields  v

let my_apply : (_, 'a -> 'b) formlet -> (_, 'a) formlet -> (_, 'b) formlet =
  fun f p -> formlet typed <#>{f => g}{p => q}</#> yields g q


(*
  The `pure' and `apply' functions using multi-holed contexts.
*)
open Formlets_contexts

let my_pure : 'a -> (_, 'a) formlet =
  fun v -> formlet contexts <#/> yields  v

let my_apply : (_, 'a -> 'b) formlet -> (_, 'a) formlet -> (_, 'b) formlet =
  fun f p -> formlet contexts <#>{f => g}{p => q}</#> yields g q

>>
