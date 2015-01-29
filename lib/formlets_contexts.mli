(*
  A version of the formlet library using statically-typed multi-holed
  contexts as described in 

    * Section 6.3 of "The Essence of Form Abstraction" (Cooper et al 2008)
    * "Many Holes in Hindley-Milner" (Lindley 2008)
*)

open Basics
open Common

(*
  XML contexts with a static count of the number of holes.
*)
type +'a context
val empty  :                                                      ('a * 'a)   context
val text   :                                            string -> ('a * 'a)   context
val xml    :                                           Xml.xml -> ('a * 'a)   context
val tag    : Xml.tag ->    Xml.attributes -> 'a        context -> 'a          context
val (++)   :            ('a * 'b) context -> ('c * 'a) context -> ('c * 'b)   context
val hole   :                                                      ('a * 'a s) context

(*
  The formlet (parameterised) idiom, using multi-holed contexts.
*)
type (+'i, +'a) formlet
val pure  :                                               'a -> ('m*'m    , 'a)     formlet
val (<*>) : ('m*'n, 'a -> 'b) formlet -> ('l*'m, 'a) formlet -> ('l*'n    , 'b)     formlet
val plug  : 'a                context -> ('a,    'b) formlet -> ('c * 'c s, 'b)     formlet
val input :                                                     ('a * 'a s, string) formlet


(* Running formlets *)
val handle : (z*z s, 'a) formlet -> ('a -> Xml.xml) -> Xml.xml
val run    : (unit -> Xml.xml) -> unit
