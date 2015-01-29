(** Desugaring XML literals and the formlet syntax to typed XML, using
    Elsman and Larsen's encoding *)

open Camlp4.PreCast
open Desugar

val o : Ast.loc -> xml -> Ast.expr -> Ast.expr

