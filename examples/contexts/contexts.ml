(*pp /usr/local/ocaml/bin/camlp4of -I ../../pp desugar.cmo desugar_typed.cmo desugar_contexts.cmo lexer.cmo parser.cmo syntax.cmo  *)

(*
  The main example from "The Essence of Form Abstraction", but desugaring formlet syntax
  to multi-holed contexts.
*)
<<
open Formlets_contexts
open Common.Xml

type date = {month : int;
             day   : int}

let make_date m d = {month = m; day = d}

(*
  A simple submit button.
*)
let submit : string -> xml =
  fun txt ->
    <button type="submit">{xml_text txt}</button>

(*
  Convert an integer to an XML text node.
*)
let xml_of_int (i : int) : xml =
  xml_text (string_of_int i)

(*
  Convert a date to an XML value.
*)
let xml_of_date {month = month; day = day} : xml =
  <#>{xml_of_int month}/{xml_of_int day}</#>

(*
  A formlet for reading an integer (as in "The Essence of Form Abstraction").
*)
let input_int : (_, int) formlet = 
  formlet contexts
    <#>{input => i}</#>
  yields int_of_string i

(*
  A formlet for reading an integer (as in "The Essence of Form Abstraction").
*)
let date : (_, date) formlet =
  formlet contexts
    <div>
       Month : {input_int => month}
       Day   : {input_int => day}
    </div>
  yields make_date month day

(*
  A travel formlet that uses two instances of `date'.
*)
let travel : (_, string * date * date) formlet =
  formlet contexts
    <#>
    Name : {input => name}
    <div>
      Arrive : {date => arrive}
      Depart : {date => depart}
    </div>
    {submit "Submit!"}
   </#>
  yields (name, arrive, depart)

(*
  A continuation function for displaying the results
  of the submission of `travel'.
*)
let display_itinerary : string * date * date -> xml =
  fun (name, arrive, depart) ->
    <html>
      <head><title>Itinerary</title></head>
      <body>
        Itinerary for: {xml_text name}
        Arriving: {xml_of_date arrive}
        Departing: {(xml_of_date depart)}
      </body>
    </html>

(*
  A full HTML page containing the `travel formlet'.
*)
let page : xml = 
  <html>
    <head>
      <title>Enter your plans</title>
    </head>
    <body>
    {handle travel display_itinerary}
    </body>
  </html>

(*
  On the first run display the page.
  On subsequent runs (resulting from form submissions) run the
  continuation associated with the form.
*)
let () = run (fun _ -> page) 
>>
