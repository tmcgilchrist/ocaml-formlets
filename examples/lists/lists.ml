(*pp /usr/local/ocaml/bin/camlp4of -I ../../pp desugar.cmo desugar_typed.cmo desugar_contexts.cmo lexer.cmo parser.cmo syntax.cmo  *)

(*
  A demonstration of using formlets to construct a form with a
  variable number of fields.
*)
<<
open Formlets

(*
  This is analogous to the standard `sequence' function for monads.
*)
let rec formlets : 'a formlet list -> 'a list formlet = function 
  | []     -> formlet <#/> yields []
  | h :: t -> formlet <#>{h => h'}{formlets t => t'}</#> yields h' :: t'
>>
