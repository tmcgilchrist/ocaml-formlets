open Basics
open Common

type environment = Common.environment

(*
  multi-holed XML contexts
*)
module XmlContext :
sig
  type item = Text of string | Tag of tag * attributes * t | Hole
  and tag = string
  and attributes = (string * string) list
  and t = item list

  val to_xml : t -> Xml.xml
  val plug : t * t list -> t
end =
struct
  type item = Text of string | Tag of tag * attributes * t | Hole
  and tag = string
  and attributes = (string * string) list
  and t = item list

  let plug (k, xs) =
    let rec plug (k, xs) =
      match k with
        | [] -> [], xs
        | z :: zs ->
            let (k, xs) = plug_item (z, xs) in
            let (k', xs) = plug (zs, xs) in
              k @ k', xs
    and plug_item (z, xs) =
      match z with
        | Text s -> [Text s], xs
        | Tag (s, attributes, k) ->
            let (k, xs) = plug (k, xs) in
              [Tag (s, attributes, k)], xs
        | Hole ->
            begin match xs with
              | [] ->
                  failwith "ran out of xml to plug in"
              | x::xs -> x, xs
            end in
    let k, xs = plug (k, xs) in
      if (xs <> []) then
        failwith "failed to plug in all the xml"
      else
        k

  let rec to_xml =
    function
      | [] -> []
      | z::zs ->
          (to_xml_item z) :: (to_xml zs)
  and to_xml_item =
    function
      | Text s ->
          Xml.Text s
      | Tag (s, attributes, k) ->
          Xml.Tag (s, attributes, to_xml k)
      | Hole ->
          failwith "cannot convert a holey context to raw XML"
end

(*
  lists of length n
*)
module NList :
sig
  type (+'i, +'a) t

  val nil : ('m*'m, 'a) t
  val cons : 'a * ('m*'n, 'a) t ->
             ('m*'n s, 'a) t
  val append : ('m*'n, 'a) t * ('l*'m, 'a) t ->
               ('l*'n, 'a) t

  val to_list : ('i, 'a) t -> 'a list
end
  =
struct
  type ('i, 'a) t = 'a list

  let nil = []
  let cons (x, xs) = x :: xs
  let append (xs, ys) = xs @ ys

  let to_list xs = xs
end


(*
  the category of statically typed multi-holed XML contexts
*)
module NContext :
sig
  (* context*)
  type +'i t

  (* context constructors *)
  val empty : ('m*'m) t
  val text : string -> ('m*'m) t
  val xml : Xml.xml -> ('m*'m) t
  val tag : XmlContext.tag -> XmlContext.attributes -> 'i t -> 'i t
  val (++) : ('m*'n) t -> ('l*'m) t -> ('l*'n) t
  val hole : ('n*'n s) t

  (* upcast to xml_context *)
  val to_xml_context : 'i t -> XmlContext.t

  (* upcast to xml *)
  val to_xml : ('m*'m) t -> Xml.xml

  (* context list *)
  type (+'i, +'j) ts

  (* context list constructors *)
  val nil : ('p*'p, 'm*'m) ts
  val cons : ('p*'q) t * ('o*'p, 'm*'n) ts -> ('o*'q, 'm*'n s) ts
  val append : ('p*'q, 'm*'n) ts * ('o*'p, 'l*'m) ts -> ('o*'q, 'l*'n) ts

  (* upcast to xml list *)
  val to_xml_list : ('p*'p, 'i) ts -> Xml.xml list

  (* plugging *)
  val plug : 'j t * ('i, 'j) ts -> 'i t
end
=
struct
  open XmlContext

  type 'i t = XmlContext.t

  let empty = []
  let text s = [Text s]
  let rec xml xs = 
    List.map (function 
                | Xml.Text s -> Text s
                | Xml.Tag (t, attrs, x) -> Tag (t, attrs, xml x)) xs
  let tag s attributes x = [Tag (s, attributes, x)]
  let (++) x y = x @ y
  let hole = [Hole]

  let to_xml_context k = k
  let to_xml x = XmlContext.to_xml x

  type ('i, 'j) ts = ('j, XmlContext.t) NList.t

  let nil = NList.nil
  let cons (x, xs) = NList.cons (to_xml_context x, xs)
  let append = NList.append
          
  let to_xml_list xs = List.map XmlContext.to_xml (NList.to_list xs)

  let plug (k, xs) =
    XmlContext.plug (k, NList.to_list xs)
end
type +'i context = 'i NContext.t
let empty = NContext.empty
let text  = NContext.text
let xml   = NContext.xml
let tag   = NContext.tag
let (++)  = NContext.(++)
let hole  = NContext.hole


(*
  The parameterised accumulation idiom over the
  category of xml lists of length n
*)
module NXmlWriter :
sig
  include PIdiom

  type 'a writer = (z*z s, 'a) t

  val plug : 'i NContext.t * ('i, 'a) t -> ('m*'m s, 'a) t
  val run : 'a writer -> Xml.xml * 'a
end =
struct
  open NContext

  type ('i, 'a) t = (z*z, 'i) NContext.ts * 'a
  type 'a writer = (z*z s, 'a) t

  let pure v = (nil, v)
  let (<*>) (xs, f) (ys, a) = (append (xs, ys), f a)

  let plug (k, (xs, v)) = (cons (plug (k, xs), nil), v)

  let run (xs, v) = (List.hd (NContext.to_xml_list xs), v)
end


(*
  The formlet idiom, using multi-holed contexts
*)
module NFormlet :
sig
  include PIdiom

  type 'a formlet = (z*z s, 'a) t

  val plug : 'i NContext.t -> ('i, 'a) t -> ('m*'m s, 'a) t
  val run  : 'a formlet -> Xml.xml * (environment -> 'a)
  val input : ('m*'m s, string) t
end =
struct
  include ComposeIP (NameGen) (ComposePI (NXmlWriter) (Environment))
  module N = NameGen
  module A = NXmlWriter
  module E = Environment

  type 'a formlet = (z*z s, 'a) t

  let plug k f = N.(<*>) (N.pure (fun v -> A.plug (k, v))) f 
  let run       v = let xml, collector = A.run (N.run v)
                    in xml, E.run collector
  let input = 
    N.(<*>) (N.pure
               (fun name ->
                  A.plug (NContext.tag "input" [("name", name)] NContext.empty,
                          A.pure (E.lookup name))))
      N.next_name
end
type (+'a, +'b) formlet = ('a, 'b) NFormlet.t
let pure  = NFormlet.pure
let (<*>) = NFormlet.(<*>)
let plug  = NFormlet.plug
let input = NFormlet.input

let handle formlet handler : Xml.xml =
  let contents, collector = NFormlet.run formlet in 
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
