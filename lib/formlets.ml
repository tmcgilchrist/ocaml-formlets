open Basics
open Common

(* 
   The formlet idiom, factored.
   (Fig. 5 of "The Essence of Form Abstraction")
*)
module Formlet :
sig
  include Idiom

  val xml    : Xml.xml -> unit t
  val text   : string -> unit t
  val tag    : Xml.tag -> Xml.attributes -> 'a t -> 'a t
  val run    : 'a t -> (Xml.xml * (environment -> 'a))
  val input  : string t
end =
struct
  module AE = Compose (XmlWriter) (Environment)
  include Compose (NameGen) (AE)
  module N = NameGen module A = XmlWriter module E = Environment
    
  let xml       x = N.pure (AE.refine (A.xml x))
  let text      s = N.pure (AE.refine (A.text s))
  let tag t ats f = N.(<*>) (N.pure (A.tag t ats)) f
  let run       v = let xml, collector = A.run (N.run v)
                    in xml, E.run collector
  let input = 
    N.(<*>) (N.pure (fun n -> A.tag "input" [("name", n)] 
                       (A.pure (E.lookup n)))) 
      N.next_name
end

include Xml

include Formlet
type +'a formlet = 'a Formlet.t

(*
  Connect a formlet with the continuation used to handle its
  submission.

  We marshal and store the continuation as a hidden field of the form
  in order to keep all the state client-side.
*)
let handle formlet handler : xml =
  let contents, collector = Formlet.run formlet in 
    fst 
      (XmlWriter.run
         (XmlWriter.tag "form"
            [("enctype", "application/x-www-form-urlencoded");
             ("action",  "#");
             ("method",  "POST")]
            (XmlWriter.tag "input"
               [("type", "hidden");
              ("name", "_k");
              ("value", pickle_cont (fun env -> handler (collector env)))]
               (XmlWriter.xml contents))))

let run = Common.run
