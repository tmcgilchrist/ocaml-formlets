(* Basic types, values, interfaces, etc. *)

let id x = x
let curry   f x y    = f (x,y)
let uncurry f (x, y) = f x y

(*
  The idiom interface
*)
module type Idiom =
sig
  type +'a t
  val pure  : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end

(*
  The composition of two idioms is an idiom
*)
module Compose (F : Idiom) (G : Idiom) : 
sig
  include Idiom with type 'a t = 'a G.t F.t
  val lift   : 'a G.t -> 'a G.t F.t
  val refine : 'a F.t -> 'a G.t F.t
end =
struct
  type 'a t = 'a G.t F.t
  let pure x = F.pure (G.pure x)
  let (<*>) f x = 
    let (<*>) = F.(<*>) in
      F.pure G.(<*>) <*> f <*> x
  let lift = F.pure
  let refine v = F.(<*>) (F.pure G.pure) v
end 

(*
  The indexed idiom interface
*)
module type XIdiom =
sig
  type (+'h, +'a) t
  val pure  : 'a -> ('h, 'a) t
  val (<*>) : ('h, 'a -> 'b) t -> ('h, 'a) t -> ('h, 'b) t
end

(* 
   Post-composing an indexed idiom with an idiom
*)
module ComposeXI (F : XIdiom) (G : Idiom)
  : XIdiom with type ('h,'a) t = ('h,'a G.t) F.t =
struct
  type ('h,'a) t = ('h,'a G.t) F.t
  let pure x = F.pure (G.pure x)
  let (<*>) f x = 
    let (<*>) = F.(<*>) in
      F.pure G.(<*>) <*> f <*> x
end 

(* 
   Pre-composing an indexed idiom with an idiom
*)
module ComposeIX (F : Idiom) (G : XIdiom)
  : XIdiom with type ('h,'a) t = ('h,'a) G.t F.t =
struct
  type ('h,'a) t = ('h,'a) G.t F.t
  let pure x = F.pure (G.pure x)
  let (<*>) f x = 
    let (<*>) = F.(<*>) in
      F.pure G.(<*>) <*> f <*> x
end 

(*
  The parameterised idiom interface
*)
module type PIdiom =
sig
  type (+'i, +'a) t
  val pure  : 'a -> ('m*'m, 'a) t
  val (<*>) : ('m*'n, 'a -> 'b) t -> ('l*'m, 'a) t -> ('l*'n, 'b) t
end

(*
  Post-composing a parameterised idiom with an idiom
*)
module ComposePI (F : PIdiom) (G : Idiom)
  : PIdiom with type ('i,'a) t = ('i,'a G.t) F.t =
struct
  type ('i,'a) t = ('i,'a G.t) F.t
  let pure x = F.pure (G.pure x)
  let (<*>) f x = 
    let (<*>) = F.(<*>) in
      F.pure G.(<*>) <*> f <*> x
end 

(*
  Pre-composing a parameterised idiom with an idiom
*)
module ComposeIP (F : Idiom) (G : PIdiom)
  : PIdiom with type ('i,'a) t = ('i,'a) G.t F.t =
struct
  type ('i,'a) t = ('i,'a) G.t F.t
  let pure x = F.pure (G.pure x)
  let (<*>) f x = 
    let (<*>) = F.(<*>) in
      F.pure G.(<*>) <*> f <*> x
end 

(*
  Type-level Peano numbers.
*)
type z
type +'a s
