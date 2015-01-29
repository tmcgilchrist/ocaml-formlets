(*pp camlp4of *)

(** Desugaring XML literals and the formlet syntax to multi-holed
    contexts, as described in Lindley's ML 08 submission *)

open Camlp4.PreCast
open Desugar

(*
  Factor a formlet expression into its component parts: 
    * the multi-holed XML context
    * the inner formlet expressions 
    * the patterns to which the formlet expressions are bound

  split X[{f1 => p1}, ... {fn => pn}]
 = X[ ], [f1,...fn] [p1,...pn]
*)
let rec split : xml -> Ast.expr * Ast.expr list * Ast.patt list = function
  | `CData (_loc, s)        -> <:expr< text $str:s$ >>, [], []
  | `Expr (_loc, e)         -> <:expr< xml $e$ >>, [], []
  | `Bind (_loc, f, p)      -> <:expr< hole >>, [f], [p]
  | `Tag (_loc, t, ats, ns) -> (let x, fs, ps = split (`Seq (_loc, ns)) in
                                  <:expr< tag $str:t$ $mk_attr_expr _loc ats$ $x$ >>,
                                  fs, ps)
  | `Seq (_loc, ns)         -> (List.fold_right 
                                  (fun n (x, fs, ps) ->
                                     let x', fs', ps' = split n in
                                       <:expr< $x'$ ++ $x$ >>,
                                       fs' @ fs,
                                       ps' @ ps)
                                  ns
                                  (<:expr< empty >>, [], []))

let o _loc (q : xml) (e : Ast.expr) : Ast.expr = 
  let context, exprs, bindings = split q in
  let f = (List.fold_right (fun p e -> <:expr< fun $p$ -> $e$ >>)
             bindings e) in
  let puref = <:expr< pure $f$ >> in
  let formlet = (List.fold_left (fun f p -> <:expr< $f$ <*> $p$ >>)
                   puref exprs) in
    <:expr< plug $context$ $formlet$ >>
