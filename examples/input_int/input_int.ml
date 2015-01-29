(*pp /usr/local/ocaml/bin/camlp4of -I ../../pp desugar.cmo desugar_typed.cmo desugar_contexts.cmo lexer.cmo parser.cmo syntax.cmo  *)

(*
  The input_int formlet from "The Essence of Form Abstraction"
*)

<<
open Formlets

let input_int : int formlet  = 
  formlet
    <#>{input => i}</#>
  yields
    int_of_string i
>>

