(*pp camlp4of *)

(* desugaring, as described in the paper *)

open Camlp4.PreCast

type xml = [ `Tag of Ast.loc * tag * attr list * xml list
           | `Seq of Ast.loc * xml list
           | `Expr of Ast.loc * Ast.expr
           | `Bind of Ast.loc * Ast.expr * Ast.patt 
           | `CData of Ast.loc * string ]
and  tag = string
and attr = string * string

let die loc msg =
  Syntax.print_warning loc msg;
  exit 1

let mk_attr_expr _loc attrs = 
  List.fold_right
    (fun (k, v) e -> <:expr< ($str:k$, $str:v$) :: $e$ >>)
    attrs
  <:expr< [] >>

let mk_expr_tuple _loc = function
  | [] -> <:expr< () >>
  | exprs ->
      List.fold_left
        (fun l r -> <:expr< $l$, $r$ >>)
        (List.hd exprs)
        (List.tl exprs)

let mk_patt_tuple _loc = function
  | [] -> <:patt< () >>
  | patts ->
      List.fold_left
        (fun l r -> <:patt< $l$, $r$ >>)
        (List.hd patts)
        (List.tl patts)

let rec patt_as_expr : Ast.patt -> Ast.expr =
  let pe = patt_as_expr in function
    | <:patt@_loc< $l$, $r$>> -> <:expr< $pe l$, $pe r$ >>
    | <:patt@_loc< ` $uid:c$ >> -> <:expr< ` $c$ >>
    | <:patt@_loc< $id:c$ >> -> <:expr< $id:c$ >>
    | Ast.PaAli (_loc, _, x) -> <:expr< $pe x$ >>
    | <:patt@_loc< ($p$ : $t$) >> -> <:expr< ($pe p$ : $t$) >>
    | <:patt@_loc< $id:l$ $r$ >> -> <:expr< $id:l$ $pe r$ >>
    | <:patt@_loc< [| $p$ |] >> -> <:expr< [| $pe p$ |] >>
    | <:patt@_loc< $int:i$ >> -> <:expr< $int:i$ >>
    | <:patt@_loc< $int32:i$ >> -> <:expr< $int32:i$ >>
    | <:patt@_loc< $int64:i$ >> -> <:expr< $int64:i$ >>
    | <:patt@_loc< $str:i$ >> -> <:expr< $str:i$ >>
    | <:patt@_loc< $flo:i$ >> -> <:expr< $flo:i$ >>
    | <:patt@_loc< $chr:i$ >> -> <:expr< $chr:i$ >>
    | <:patt@_loc< $nativeint:i$ >> -> <:expr< $nativeint:i$ >>
    | <:patt@_loc<  >> -> <:expr<  >>
    | Ast.PaCom (_loc, l, r) -> Ast.ExCom (_loc, pe l, pe r)
    | Ast.PaApp (_loc, l, r) -> Ast.ExApp (_loc, pe l, pe r)
    | Ast.PaTup (_loc, p) -> Ast.ExTup (_loc, pe p)
    | Ast.PaSem (_loc, l, r) -> Ast.ExSem (_loc, pe l, pe r)
    | <:patt@_loc< _ >> -> die _loc "Wildcard patterns not currently supported" 
    | <:patt@_loc< { $pat:p$ } >> -> die _loc "Record bindings not currently supported" 
    | <:patt@_loc< # $_$ >> -> die _loc "Hash-patterns not currently supported" 
    | Ast.PaRng (_loc, _, _) -> die _loc "Range patterns are not supported" 
    | Ast.PaOrp (_loc, _, _) -> die _loc "Disjunctive patterns are not supported" 
    | Ast.PaOlbi (_loc, _, _, _) | Ast.PaOlb (_loc, _, _) | Ast.PaLab (_loc, _, _) ->
        die _loc "Label patterns are not supported"
    | Ast.PaEq (_loc, _, _) | Ast.PaAnt (_loc, _) -> die _loc "This pattern is not currently supported"

let rec f : xml -> Ast.expr = function
  | `CData (_loc, s)         -> <:expr< text $str:s$ >>
  | `Expr (_loc, e)          -> <:expr< xml $e$ >>
  | `Bind (_loc, f, p)       -> <:expr< $f$ >>
  | `Tag (_loc, t, ats, ns)  -> 
      <:expr< tag $str:t$ $mk_attr_expr _loc ats$ $f (`Seq (_loc, ns))$ >>
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
and dagger_p : xml -> Ast.patt list = function
  | `CData (_loc, _)   -> [ <:patt< () >> ]
  | `Expr (_loc, _)    -> [ <:patt< () >> ]
  | `Bind (_loc, _, p) -> [p]
  | `Tag (_, _, _, ns) 
  | `Seq (_, ns)       -> List.concat (List.map dagger_p ns)
and dagger_e : xml -> Ast.expr list = function
  | `CData (_loc, _)   -> [ <:expr< () >> ]
  | `Expr (_loc, _)    -> [ <:expr< () >> ]
  | `Bind (_loc, _, p) -> [patt_as_expr p]
  | `Tag (_, _, _, ns) 
  | `Seq (_, ns)       -> List.concat (List.map dagger_e ns)

let o _loc (q : xml) (e : Ast.expr) : Ast.expr = 
  let qdagger = mk_patt_tuple _loc (dagger_p q )
  and qf = f q in
    <:expr< (<*>) (pure (fun $qdagger$ -> $e$)) $qf$ >>

let rec x = function
  | `Tag (_loc, t, ats, ns) ->
      <:expr< xml_tag $str:t$ $mk_attr_expr _loc ats$ $x (`Seq (_loc, ns))$ >>
  | `Seq (_loc, ns) -> (List.fold_right
                          (fun n e -> <:expr< $x n$ @ $e$>>)
                          ns
                          <:expr< [] >>)
  | `Expr (_loc, e) -> e
  | `Bind (_loc, f, p) -> die _loc "Formlet bindings are not allowed in plain XML literals"
  | `CData (_loc, s) -> <:expr< xml_text $str:s$ >>
