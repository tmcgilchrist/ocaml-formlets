(*
  Register our expander as the default quotation expander.
*)

module S = Camlp4.PreCast.Syntax
module P = Parser.Make(S)

let () = S.Quotation.add "formlets" S.Quotation.DynAst.str_item_tag P.formlet_str_item
let () = S.Quotation.default := "formlets"
