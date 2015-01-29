(** Desugaring XML literals and the formlet syntax to multi-holed
    contexts, as described in Lindley's ML 08 submission *)

open Camlp4.PreCast
open Desugar

val o : Ast.loc -> xml -> Ast.expr -> Ast.expr

