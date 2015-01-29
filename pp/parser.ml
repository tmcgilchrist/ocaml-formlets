(*pp camlp4of *)
module Lex = Lexer.Make(Camlp4.PreCast.Token)

open Camlp4.PreCast
open Desugar

module Make (Syntax : Camlp4.Sig.Camlp4Syntax 
                      with module Loc   = Loc
                       and module Ast   = Ast
                       and module Token = Token
                       and module Gram  = Gram) =
struct

  let fatal_error loc fmt = 
    let die msg = 
      let () = Syntax.print_warning loc msg in
        exit 1 in
      Printf.kprintf die fmt

  module S = Syntax

  (*
    The various translations for desugaring formlet syntax.
  *)
  let desugarers = 
    [("standard", Desugar.o);
     ("",         Desugar.o);
     ("typed",    Desugar_typed.o);
     ("contexts", Desugar_contexts.o)]

  (*
    Extend the (regular OCaml) grammar with productions for
    parsing formlet syntax and XML literals
  *)
  EXTEND S.Gram
    GLOBAL:
      S.expr
      S.ctyp
      S.a_LIDENT
      S.a_STRING
      S.a_LABEL
    ;
  
    endtag:
    [[ "</"; id = S.a_LIDENT; ">" -> id ]];
  
    xmllit:
    [[":xml"; x = xml -> x]];

    xml:
    [[ "<#/>" -> `Seq (_loc, [])
     | "<#>";  contents = LIST0 xml_contents; "</#>" -> `Seq (_loc, contents)
     | id = S.a_LABEL; attrs = LIST0 attr; "/>" ->
         (* start-end tag, end tag, attrs *) 
         `Tag (_loc, id, attrs, [])
     | id = S.a_LABEL; attrs = LIST0 attr; ">"; contents = LIST0 xml_contents; e = endtag ->
         (* start tag, end tag, attrs, contents *)
         if id <> e
         then fatal_error _loc "Start tag (%s) does not match end tag (%s)" id e
         else `Tag (_loc, id, attrs, contents) ]];
  
    splice_rest:
    [[ "FORMLET_ARROW"; p = S.patt; "}" -> `Patt p
     | "}" -> `Close ]];

    splice:
    [[ "{"; e = S.expr LEVEL "top"; r = splice_rest ->
     match r with
     | `Close -> `Expr (_loc, e)
     | `Patt p -> `Bind (_loc, e, p) ]];

    xml_contents:
    [[ c = cdata           -> c
     | s = splice          -> s
     | x = xml             -> x ]];

    cdata:
    [[ s = S.a_STRING -> `CData (_loc, s) ]];
  
    attr:
    [[ k = S.a_STRING; "="; v = S.a_STRING -> (k, v) ]];
  
    S.expr:
    [[ "formlet"; d = opt_lident; q = xmllit; "yields"; e = Syntax.expr LEVEL "top" ->
         let o = try List.assoc d desugarers 
                 with Not_found -> fatal_error _loc "Unknown desugarer: %s" d
         in o _loc q e
     | qx = xmllit -> Desugar.x qx ]];

    (* This allows us to use "formlet" both as an expression-level
       keyword and as a type constructor *)
    S.ctyp: LEVEL "ctyp1"
    [[ t1 = SELF; "formlet" -> <:ctyp< $t1$ formlet >>]];

    S.ctyp: LEVEL "simple"
    [[ "("; t = SELF; ","; mk = comma_ctyp_app; ")"; ty = tycon -> mk <:ctyp< $t$ $ty$ >> ]];

    tycon:
    [[ "formlet" -> <:ctyp< formlet >>
     | i = S.ctyp LEVEL "ctyp2" -> i ]];

    comma_ctyp_app:
      [ [ t1 = S.ctyp; ","; t2 = SELF -> fun acc -> t2 <:ctyp< $t1$ $acc$ >>
        | t = S.ctyp -> fun acc -> <:ctyp< $t$ $acc$ >>
      ] ]
    ;

    opt_lident:
    [[ i = S.a_LIDENT -> i
     |                -> "" ]];
    END

  let trace_stream : ('a -> unit) -> 'a Stream.t -> 'a Stream.t =
    fun f stream -> 
      Stream.from
        (fun _ -> 
           match Stream.peek stream with
             | Some t -> let () = Stream.junk stream in 
               let () = f t in 
                 Some t
             | None   -> None)

  let trace_token_stream : (Token.t * Loc.t) Stream.t -> (Token.t * Loc.t) Stream.t =
    let print_token (t, _) = 
      Printf.fprintf stderr "T: %s\n" (Token.to_string t) in
      trace_stream print_token

  let trace_filtered_token_stream : (Token.t * Loc.t) Stream.t -> (Token.t * Loc.t) Stream.t =
    let print_token (t, _) = 
      Printf.fprintf stderr "F:\t\t\t%s\n" (Token.to_string t) in
      trace_stream print_token

  let parse = Lex.from_string ~quotations:false
    
  (* This is a module-signature-abstraction-breaking cast, not a more
     dangerous representation-inspection cast *)
  let filter_cast : 'a -> 'a Gram.not_filtered = Obj.magic
    
  let to_str_item (stream : (Token.t * Loc.t) Stream.t) (loc : Loc.t) =
    (* here we just invoke the standard str_item after filtering the
    token stream.  Filtering performs various transformations
    including recognising keywords and removing blank spaces *)
    let filtered = Gram.filter (filter_cast stream) in
    let traced   = if Lex.debugging then trace_filtered_token_stream filtered
                   else filtered in
      Syntax.Gram.parse_tokens_after_filter Syntax.str_items traced

  let formlet_str_item loc _ s = 
    let parsed = parse loc s in
    let traced = if Lex.debugging then trace_token_stream parsed
                 else parsed in
      to_str_item traced loc
end
