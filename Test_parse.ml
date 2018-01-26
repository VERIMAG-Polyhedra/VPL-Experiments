open Interpreter;;
open Domains;;
open IOBuild;;

folder := "/home/amarecha/VPL-Experiments/benchs/";
print_endline "Parsing test_c.c";;

(* Parsing file *)
let defs = match Frontc.parse_file "test_c.c" Pervasives.stdout with
| Frontc.PARSING_ERROR ->
 	Pervasives.failwith "FrontC Parsing Error"
| Frontc.PARSING_OK defs -> defs
;;

open Domains.Printer_Domain;;

Vpl.Debug.enable();;
Vpl.Debug.print_enable();;
Vpl.Debug.set_colors();;


let main_fun = List.find (function
	| Cabs.FUNDEF((_,_,(name,_,_,_)), body) when String.equal name "main" -> true
	| _ -> false)
	defs
in
(* Printing definitions *)
(*List.iter Cprint.print_def defs;*)
match main_fun with
| Cabs.FUNDEF((_,_,(name,_,_,_)), body) ->
	let (mem,stmt) = Stmt.from_body MapS.empty body in
	Vpl.Misc.list_to_string
		(fun (name,value) -> Printf.sprintf "%s -> %s" name (Stmt.Value.to_string value))
		(MapS.bindings mem)
		" ; "
	|> print_endline;
	let _ = run mem stmt in
	()
| _ -> ()
;;
