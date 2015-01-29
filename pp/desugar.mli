(** Desugaring XML literals and the formlet syntax.  The functions "o"
    and "x" correspond to (-)^o and [[-]]_x in "The Essence of Form
    Abstraction". 
 *)

open Camlp4.PreCast

type xml = [ `Tag of Ast.loc * tag * attr list * xml list
           | `Seq of Ast.loc * xml list
           | `Expr of Ast.loc * Ast.expr
           | `Bind of Ast.loc * Ast.expr * Ast.patt 
           | `CData of Ast.loc * string ]
and  tag = string
and attr = string * string

val o : Ast.loc -> xml -> Ast.expr -> Ast.expr
val x :            xml             -> Ast.expr

(* Some utility functions that we use in other desugarers *)

val mk_expr_tuple : Ast.loc -> Ast.expr list -> Ast.expr
val mk_patt_tuple : Ast.loc -> Ast.patt list -> Ast.patt
val mk_attr_expr  : Ast.loc -> (string * string) list -> Ast.expr
val patt_as_expr : Ast.patt -> Ast.expr
val dagger_p : xml -> Ast.patt list
val dagger_e : xml -> Ast.expr list
val die : Ast.loc -> string -> 'a
