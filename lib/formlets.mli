(*
  The formlet library, as described in 
  "The Essence of Form Abstraction" (Cooper et al 2008)
*)

open Basics
open Common
open Xml

(*
  Xml constructors
*)
val xml_text :                       string -> xml
val xml_tag  : tag -> attributes ->     xml -> xml

(*
  The formlet idiom
*)
type +'a formlet

(* formlet constructors *)
val pure  :                                      'a -> 'a     formlet
val (<*>) :        ('a -> 'b) formlet -> 'a formlet -> 'b     formlet
val xml   :                                     xml -> unit   formlet
val text  :                                  string -> unit   formlet
val tag   : tag ->         attributes -> 'a formlet -> 'a     formlet
val input :                                            string formlet

(* Running formlets *)
val handle : 'a formlet -> ('a -> xml) -> xml
val run    : (unit -> xml) -> unit
