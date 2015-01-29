(*pp camlp4of *)

(** Desugaring XML literals and the formlet syntax to typed XML, using
    Elsman and Larsen's encoding *)

open Camlp4.PreCast
open Desugar

let mk_attr_expr _loc tag attrs = 
  let attr_module = String.uppercase tag ^ "AttrSet" in
  let attrs =
    List.fold_right
      (fun (k, v) e -> <:expr< (`$uid:String.uppercase k$, $str:v$) :: $e$ >>)
      attrs
    <:expr< [] >> in
    <:expr< $uid:attr_module$.of_list $attrs$ >>


let rec f : xml -> Ast.expr = function
  | `CData (_loc, s)         -> <:expr< text $str:s$ >>
  | `Expr (_loc, e)          -> <:expr< xml $e$ >>
  | `Bind (_loc, f, p)       -> <:expr< $f$ >>
  | `Tag (_loc, t, ats, ns)  -> 
      <:expr< $lid:String.lowercase t$ $mk_attr_expr _loc t ats$ $f (`Seq (_loc, ns))$ >>
  | `Seq (_loc, ns)   -> 
      let ndaggers = List.map dagger_p ns in
      let body = mk_expr_tuple _loc (List.map patt_as_expr (List.concat ndaggers)) in
      let func = (List.fold_right
                    (fun p e -> <:expr< fun $mk_patt_tuple _loc p$ -> $e$ >>)
                    ndaggers
                    body) in
      let puref = <:expr< pure $func$ >>   in
        List.fold_left
          (fun f p -> <:expr<  $f$ <*> $p$ >>)
          puref
          (List.map f ns)

let o _loc (q : xml) (e : Ast.expr) : Ast.expr = 
  let qdagger = mk_patt_tuple _loc (dagger_p q )
  and qf = f q in
    <:expr< (<*>) (pure (fun $qdagger$ -> $e$)) $qf$ >>
