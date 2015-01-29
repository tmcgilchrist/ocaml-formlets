open Basics
open Common

(*
  A set of XML attributes, whose keys are polymorphic variant labels
  and whose values are strings.
*)
module AttrSet =
struct
  module type S =
  sig
    include Set.S
    val attr_list : t -> (string * string) list
    val of_list : elt list -> t
  end

  module Make (M : sig
                     type t
                     val to_string : t -> string
                   end) :
    S with type elt = M.t * string = 
  struct
    include Set.Make 
      (struct
         type t = M.t * string
         let compare (x, _) (y, _) = compare x y
       end)
    let attr_list set =
      fold (fun (k,v) l -> (M.to_string k,v) :: l) set []
    let of_list list =
      List.fold_right add list empty
  end
end


(*
   Attribute sets for each of the XHTML elements.  Only the attributes
   listed in the type `elt' are permitted to appear in the
   corresponding element: for example, only "name", "type" and "value"
   are allowed (in our library) as attributes of "input".  In the
   current version we do not allow attributes at all, except for
   <form> and <input> elements.
*)
module INPUTAttrSet = 
  AttrSet.Make (struct type t = [ `NAME | `TYPE | `VALUE ]
                       let to_string = function
                         | `NAME -> "name"
                         | `TYPE -> "type"
                         | `VALUE -> "value"
                end)

module FORMAttrSet = 
  AttrSet.Make (struct type t = [`METHOD | `ACTION | `ENCTYPE] 
                       let to_string = function
                         | `METHOD -> "method"
                         | `ACTION -> "action"
                         | `ENCTYPE -> "enctype"
                end)

module NoAttrSet = 
  AttrSet.Make (struct type t = private [>  ]
                       let to_string = function _ -> assert false
                end)

module PAttrSet     = NoAttrSet
module EMAttrSet    = NoAttrSet
module PREAttrSet   = NoAttrSet
module BIGAttrSet   = NoAttrSet
module TABLEAttrSet = NoAttrSet
module TRAttrSet    = NoAttrSet
module TDAttrSet    = NoAttrSet

(*
  Type-level markers for recording properties of XML values
  (such as whether the value denotes an inline or block element).
  For the details, see Elsman and Larsen (2004).
*)
type (+'blk, +'inl) flw
and tr
and td
and blk
and inl
and no
and inpre
and preclosed

(* 
   MiniXHTML plus <form> and <input> and some basic support for attributes
*)
module WeeXHTML :
sig
  (* contexts *)
  type +'h t

  (* context constructors *)
  val text   : string                    -> 'h t
  val p      : PAttrSet.t -> ((no, inl) flw * 'c) t    -> ((blk,'b) flw * 'c) t
  val em     : EMAttrSet.t -> ((no, inl) flw * 'c) t    -> (('b, inl) flw * 'c) t
  val pre    : PREAttrSet.t -> ((no, inl) flw * inpre) t -> ((blk, 'b) flw * 'c) t
  val big    : BIGAttrSet.t -> ((no, inl) flw * 'c) t    -> (('b, inl) flw * preclosed) t
  val table  : TABLEAttrSet.t -> (tr * 'c) t               -> ((blk, 'b) flw * 'c) t
  val tr     : TRAttrSet.t -> (td * 'c) t               -> (tr * 'c) t
  val td     : TDAttrSet.t -> ((blk, inl) flw * 'c) t   -> (td * 'c) t

  val form   : FORMAttrSet.t ->  ((no, inl) flw * 'c) t ->  ((blk,'b) flw* 'c) t
  val input  : INPUTAttrSet.t -> ((no, inl) flw * 'c) t -> (('b, inl) flw * 'c) t

  val empty  : 'h t
  val concat : 'h t -> 'h t -> 'h t

  (* upcast to xml *)
  val to_xml : 'h t -> Xml.xml
end
  =
struct
  type preclosed

  type 'h t = Xml.xml

  open Xml

  let text         s = [Text s]
  let p      attrs x = [Tag ("p",     PAttrSet.attr_list attrs, x)]
  let em     attrs x = [Tag ("em",    PAttrSet.attr_list attrs, x)]
  let pre    attrs x = [Tag ("pre",   PREAttrSet.attr_list attrs, x)]
  let big    attrs x = [Tag ("big",   BIGAttrSet.attr_list attrs, x)]
  let table  attrs x = [Tag ("table", TABLEAttrSet.attr_list attrs, x)]
  let tr     attrs x = [Tag ("tr",    TRAttrSet.attr_list attrs, x)]
  let td     attrs x = [Tag ("td",    TDAttrSet.attr_list attrs, x)]
  let form   attrs x = [Tag ("form",  FORMAttrSet.attr_list attrs, x)]
  let input  attrs x = [Tag ("input", INPUTAttrSet.attr_list attrs, x)]
  let concat   = (@)
  let empty    = []
  let to_xml   = id
end

module WX = WeeXHTML

(* 
   The accumulation idiom over the monoid of typed XML forests.
*)
module XmlWriter :
sig
  open WX
  include XIdiom

  val xml    : 'h WX.t -> ('h, unit) t
  val text   : string -> ('h, unit) t
  val p      : PAttrSet.t -> ((no, inl) flw * 'c, 'a) t    -> ((blk,'b) flw * 'c, 'a) t
  val em     : EMAttrSet.t -> ((no, inl) flw * 'c, 'a) t    -> (('b, inl) flw * 'c, 'a) t
  val pre    : PREAttrSet.t -> ((no, inl) flw * inpre, 'a) t -> ((blk, 'b) flw * 'c, 'a) t
  val big    : BIGAttrSet.t -> ((no, inl) flw * 'c, 'a) t    -> (('b, inl) flw * preclosed, 'a) t
  val table  : TABLEAttrSet.t -> (tr * 'c, 'a) t               -> ((blk, 'b) flw * 'c, 'a) t
  val tr     : TRAttrSet.t -> (td * 'c, 'a) t               -> (tr * 'c, 'a) t
  val td     : TDAttrSet.t -> ((blk, inl) flw * 'c, 'a) t   -> (td * 'c, 'a) t
  val form   : FORMAttrSet.t ->  ((no, inl) flw * 'c, 'a) t -> ((blk,'b) flw* 'c, 'a) t
  val input  : INPUTAttrSet.t -> ((no, inl) flw * 'c, 'a) t -> (('b, inl) flw * 'c, 'a) t

  val run  : ('h, 'a) t -> 'h WX.t * 'a 
end =
struct
  open WX
  type (+'h, +'a) t = 'h WX.t * 'a

  let pure v = (empty, v)
  let (<*>) (x,f) (y,a) = (concat x y, f a)

  let xml    x = (x, ())

  let first f (x, y) = (f x, y)

  let text         s = xml (text s)
  let p      attrs = first (p attrs)
  let em     attrs = first (em attrs)
  let pre    attrs = first (pre attrs)
  let big    attrs = first (big attrs)
  let table  attrs = first (table attrs)
  let tr     attrs = first (tr attrs)
  let td     attrs = first (td attrs)
  let form   attrs = first (form attrs)
  let input  attrs = first (input attrs) 
  let run      = id
end

(* 
   The formlet idiom, using statically-typed XHTML.
 *)
module Formlet :
sig
  open WX
  include XIdiom

  val xml    : 'h WX.t -> ('h, unit) t
  val text   : string -> ('h, unit) t
  val p      : PAttrSet.t -> ((no, inl) flw * 'c, 'a) t    -> ((blk,'b) flw * 'c, 'a) t
  val em     : EMAttrSet.t -> ((no, inl) flw * 'c, 'a) t    -> (('b, inl) flw * 'c, 'a) t
  val pre    : PREAttrSet.t -> ((no, inl) flw * inpre, 'a) t -> ((blk, 'b) flw * 'c, 'a) t
  val big    : BIGAttrSet.t -> ((no, inl) flw * 'c, 'a) t    -> (('b, inl) flw * preclosed, 'a) t
  val table  : TABLEAttrSet.t -> (tr * 'c, 'a) t               -> ((blk, 'b) flw * 'c, 'a) t
  val tr     : TRAttrSet.t -> (td * 'c, 'a) t               -> (tr * 'c, 'a) t
  val td     : TDAttrSet.t -> ((blk, inl) flw * 'c, 'a) t   -> (td * 'c, 'a) t
  val form   : FORMAttrSet.t ->  ((no, inl) flw * 'c, 'a) t -> ((blk,'b) flw* 'c, 'a) t

  val input  : ((no, inl) flw * 'c, unit) t -> (('b, inl) flw * 'c, string) t

  val run    : ('h, 'a) t -> 'h WX.t * (environment -> 'a)
end =
struct
  include ComposeXI (ComposeIX (NameGen) (XmlWriter)) (Environment)
  module N = NameGen
  module A = XmlWriter
  module E = Environment
    
  let xml x  = N.pure (A.(<*>) (A.pure E.pure) (A.xml x))
  let text s = N.pure (A.(<*>) (A.pure E.pure) (A.text s))
  let tag t v = N.(<*>) (N.pure t) v

  open XmlWriter

  let p     attrs v = tag (p attrs) v
  let em    attrs v = tag (em attrs) v
  let pre   attrs v = tag (pre attrs) v
  let big   attrs v = tag (big attrs) v
  let table attrs v = tag (table attrs) v
  let tr    attrs v = tag (tr attrs) v
  let td    attrs v = tag (td attrs) v
  let form  attrs v = tag (form attrs)  v

  let run v = let xml, collector = A.run (N.run v)
              in xml, E.run collector
  let input children = 
    (N.(<*>)
       (N.(<*>) (N.pure
                   (fun name children ->
                      A.input (INPUTAttrSet.of_list [(`NAME, name)])
                        (A.(<*>)
                           (A.pure (fun _ -> (E.lookup name))) 
                           children)))
          N.next_name)
       children)
end

include Formlet
type (+'h, +'a) formlet = ('h, 'a) Formlet.t

(*
  Connect a formlet with the continuation used to handle its
  submission.

  We marshal and store the continuation as a hidden field of the form
  in order to keep all the state client-side.
*)
let handle formlet handler : Xml.xml =
  let contents, collector = Formlet.run formlet in 
    WX.to_xml
      (fst 
         (XmlWriter.run
            (XmlWriter.form
               (FORMAttrSet.of_list
                  [(`ENCTYPE, "application/x-www-form-urlencoded");
                   (`ACTION,  "#");
                   (`METHOD,  "POST")])
               (XmlWriter.input
                  (INPUTAttrSet.of_list
                     [(`TYPE, "hidden");
                      (`NAME, "_k");
                      (`VALUE, pickle_cont (fun env -> handler (collector env)))])
                  (XmlWriter.xml contents)))))

let run = Common.run
