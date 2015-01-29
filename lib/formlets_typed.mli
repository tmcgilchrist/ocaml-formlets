(*
  A version of the formlet library using statically-typed XML
  as described in 

    * Section 6.1 of "The Essence of Form Abstraction" (Cooper et al 2008)
    * "Typing XHTML web applications in ML" (Elsman and Larsen 2004) for the XML typing.

  We handle only the following XHTML elements:
  <p>, <em>, <pre>, <big>, <table>, <tr>, <td>, <form> and <input>.
*)
open Basics
open Common

(*
  A set of XML attributes, whose keys are polymorphic variant labels
  and whose values are strings.
*)
module AttrSet :
sig
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
    S with type elt = M.t * string
end

(*
   Attribute sets for each of the XHTML elements.  Only the attributes
   listed in the type `elt' are permitted to appear in the
   corresponding element: for example, only "name", "type" and "value"
   are allowed (in our library) as attributes of "input".  In the
   current version we do not allow attributes at all, except for
   <form> and <input> elements, which are only used internally, so all
   these are trivial.
*)
module PAttrSet     : AttrSet.S
module EMAttrSet    : AttrSet.S
module PREAttrSet   : AttrSet.S
module BIGAttrSet   : AttrSet.S
module TABLEAttrSet : AttrSet.S
module TRAttrSet    : AttrSet.S
module TDAttrSet    : AttrSet.S

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
  Our slight extension of Elsman and Larsen's MiniXHTML.
  For the details, see Elsman and Larsen (2004).
*)
module WeeXHTML :
sig
  type +'a t
  val text   : string -> 'a t
  val p      : PAttrSet.t -> ((no, inl) flw * 'c) t    -> ((blk,'b) flw * 'c) t
  val em     : EMAttrSet.t -> ((no, inl) flw * 'c) t    -> (('b, inl) flw * 'c) t
  val pre    : PREAttrSet.t -> ((no, inl) flw * inpre) t -> ((blk, 'b) flw * 'c) t
  val big    : BIGAttrSet.t -> ((no, inl) flw * 'c) t    -> (('b, inl) flw * preclosed) t
  val table  : TABLEAttrSet.t -> (tr * 'c) t               -> ((blk, 'b) flw * 'c) t
  val tr     : TRAttrSet.t -> (td * 'c) t               -> (tr * 'c) t
  val td     : TDAttrSet.t -> ((blk, inl) flw * 'c) t   -> (td * 'c) t
  val empty  : 'a t
  val concat : 'a t -> 'a t -> 'a t
end

(*
  The formlet (indexed) idiom, using statically-typed XHTML.
*)
type (+'h, +'a) formlet

(** constructing and composing formlets *)
val pure  : 'a -> ('h, 'a) formlet
val (<*>) : ('h, 'a -> 'b) formlet -> ('h, 'a) formlet -> ('h, 'b) formlet
val xml    : 'h WeeXHTML.t -> ('h, unit) formlet
val text   : string -> ('h, unit) formlet
val p      : PAttrSet.t     -> ((no, inl) flw * 'c, 'a) formlet    -> ((blk,'b) flw * 'c, 'a) formlet
val em     : EMAttrSet.t    -> ((no, inl) flw * 'c, 'a) formlet    -> (('b, inl) flw * 'c, 'a) formlet
val pre    : PREAttrSet.t   -> ((no, inl) flw * inpre, 'a) formlet -> ((blk, 'b) flw * 'c, 'a) formlet
val big    : BIGAttrSet.t   -> ((no, inl) flw * 'c, 'a) formlet    -> (('b, inl) flw * preclosed, 'a) formlet
val table  : TABLEAttrSet.t -> (tr * 'c, 'a) formlet               -> ((blk, 'b) flw * 'c, 'a) formlet
val tr     : TRAttrSet.t    -> (td * 'c, 'a) formlet               -> (tr * 'c, 'a) formlet
val td     : TDAttrSet.t    -> ((blk, inl) flw * 'c, 'a) formlet   -> (td * 'c, 'a) formlet
val input  :                   ((no, inl) flw * 'c, unit) formlet -> (('b, inl) flw * 'c, string) formlet

(** Running formlets *)
val handle : ((no, inl) flw * 'a, 'b) formlet -> ('b -> Xml.xml) -> Xml.xml
val run : (unit -> Xml.xml) -> unit
