open Interpreter;;
open Domains;;
open IOBuild;;
open Vpl;;
open Arg;;

(* *************************************** *)
(* ************** Input file ************* *)
(* *************************************** *)


module Cmd = struct

	let file = ref ""

	let output_file = ref None

	let debug = ref false

	let time_budget : int option ref = ref None

	let flag_lp = ref Flags.Splx

	let flag_plp = ref Flags.Adj_Raytracing

	let flag_scalar = ref Flags.Rat

	(* These flags are functions as they depend on the value of flag_scalar or flag_lp. *)
	let flag_min = ref (fun () -> Flags.Classic)

	let flag_proj = ref (fun () -> Flags.FM)

	let flag_join = ref (fun () -> Flags.Baryc)

	let update_lp = function
		| "glpk" -> flag_lp := Flags.Glpk
		| "splx" -> flag_lp := Flags.Splx
		| _ -> invalid_arg "update_lp"

	let update_plp = function
		| "raytracing" -> flag_plp := Flags.Adj_Raytracing
		| "greedy" -> flag_plp := Flags.Greedy
		| _ -> invalid_arg "update_plp"

	let update_scalar = function
		| "rat" -> flag_scalar := Flags.Rat
		| "symb" -> flag_scalar := Flags.Symbolic
		| "float" -> flag_scalar := Flags.Float
		| _ -> invalid_arg "update_scalar"

	let update_proj = function
		| "fm" -> flag_proj := (fun () -> Flags.FM)
		| "plp" -> flag_proj := (fun () -> Flags.Proj_PLP !flag_scalar)
		| _ -> invalid_arg "update_proj"

	let update_join = function
		| "barycentric" -> flag_join := (fun () -> Flags.Baryc)
		| "plp" -> flag_join := (fun () -> Flags.Join_PLP !flag_scalar)
		| "plp_regions" -> flag_join := (fun () -> Flags.Join_fromRegions)
		| _ -> invalid_arg "update_join"

	let update_min = function
		| "classic" -> flag_min := (fun () -> Flags.Classic)
		| "raytracing" -> flag_min := (fun () -> Flags.Raytracing !flag_lp)
		| _ -> invalid_arg "update_min"

	let spec_list = [
		("-file", String (fun s -> file:=s), Printf.sprintf "<file> input file (default=%s)" !file);
		("-res", String (fun s -> output_file:= Some s),"<file> output file (default=None)");
		("-debug", Unit (fun () -> debug:= true ),"Enable debug mode");
		("-timeout", Int (fun i -> time_budget := Some i ),"Timeout value");
		("-folder", String (fun s -> folder := s), "Folder containing polyhedra files");
		("-lp", String update_lp, "LP method (gplk | splx)");
		("-plp", String update_plp, "PLP method (raytracing | greedy)");
		("-scalar", String update_scalar, "Scalar type (rat | symb | float)");
		("-proj", String update_proj, "Projection algorithm (fm | plp)");
		("-join", String update_join, "Join algorithm (barycentric | plp | plp_regions)");
		("-min", String update_min, "Minimization algorithm (classic | raytracing)");
	]

	let anon_fun s = failwith ("unsupported argument "^s);;

	let usage_msg = "VPL trace runner. Available options:";;

end

open Cmd;;

Arg.parse spec_list anon_fun usage_msg;;

Printf.printf "Input file : %s\n" (!file);;

let print_res : string -> unit
	= fun s ->
	match !output_file with
	| None -> ()
	| Some resFile ->
		let chRes = Pervasives.open_out_gen [Open_creat ; Open_wronly ; Open_append] 0o640 (resFile) in
		Pervasives.output_string chRes s;
		Pervasives.close_out chRes
;;


(* *************************************** *)
(* *************** Timeout *************** *)
(* *************************************** *)

exception Timeout

let timeout_handler : int -> unit
  =	fun _ -> Pervasives.raise Timeout

(* *************************************** *)
(* *************** Modules *************** *)
(* *************************************** *)

module VPL = struct

	include CWrappers.Interface(Scalar.Rat)

	exception Out_of_Scope

	module M = Map.Make(struct type t = string let compare = Pervasives.compare end)

	(* Map associating variable names (as string) to their VPL representation. *)
	let mapVar : Var.Positive.t M.t ref = ref M.empty
	let next : Var.Positive.t ref = ref Var.Positive.u

	let rec to_term : Cabs.expression -> Term.t
		= Cabs.(Term.(function
		| UNARY (MINUS, e) -> Opp (to_term e)
		| UNARY (PLUS, e) -> to_term e
		| UNARY (POSINCR, e) -> Add (to_term e, Cte Scalar.Rat.u)
		| UNARY (POSDECR, e) -> Add (to_term e, Cte Scalar.Rat.negU)
		| BINARY (ADD, e1, e2) -> Add (to_term e1, to_term e2)
		| BINARY (MUL, e1, e2) -> Mul (to_term e1, to_term e2)
		| BINARY (SUB, e1, e2) -> Add (to_term e1, Opp (to_term e2))
		| CONSTANT (CONST_INT c) -> Cte (Scalar.Rat.of_string c)
		| CONSTANT (CONST_FLOAT f) -> begin
			print_endline f;
			Cte (Scalar.Rat.of_string f)
			end
		| VARIABLE var_name -> begin
			if M.mem var_name !mapVar
			then Var (M.find var_name !mapVar)
			else begin
				let value = !next in
				M.add var_name !next !mapVar;
				next := Var.Positive.next !next;
				Var value
			end
			end
		| _ -> Pervasives.raise Out_of_Scope
		))

	let rec to_cond : Cabs.expression -> Cond.t
		= Cabs.(Cond.(Vpl.Cstr.(Vpl.CWrappers.(function
		| NOTHING -> Basic true
		| UNARY (NOT, e) -> Not (to_cond e)
		| BINARY (AND, e1, e2)
		| BINARY (BAND, e1, e2) -> BinL (to_cond e1, AND, to_cond e2)
		| BINARY (OR, e1, e2)
		| BINARY (BOR, e1, e2) -> BinL (to_cond e1, OR, to_cond e2)
		| BINARY (EQ, e1, e2) -> Atom (to_term e1, EQ, to_term e2)
		| BINARY (NE, e1, e2) -> Atom (to_term e1, NEQ, to_term e2)
		| BINARY (LE, e1, e2) -> Atom (to_term e1, LE, to_term e2)
		| BINARY (LT, e1, e2) -> Atom (to_term e1, LT, to_term e2)
		| BINARY (GT, e1, e2) -> Atom (to_term e1, GT, to_term e2)
		| BINARY (GE, e1, e2) -> Atom (to_term e1, GE, to_term e2)
		| _ -> Pervasives.raise Out_of_Scope
		))))

	module D = struct

		include NCDomain.NCVPL_Unit.Q

		module Interval = NCDomain.NCVPL_Unit.I.QInterface.Interval

		let name = "VPL"

		let print p = to_string Pol.Var.to_string p
			|> print_endline

		let minimize x = x

		let project = projectM

		let assume e = assume (to_cond e)

		let assign l = assign (List.map
			(fun (var, e) -> (M.find var !mapVar, to_term e))
			l)

		let project vars = project (List.map
			(fun var -> M.find var !mapVar)
			vars)

		let itvize state e = itvize state (to_term e)

	end

	include Run_Domain(D)

	let apply_flags = fun () ->
		begin
			Flags.min := !flag_min ();
			Flags.proj := !flag_proj ();
			Flags.join := !flag_join ();
			Flags.plp := !flag_plp
		end

	(*
	let to_xml : unit -> string
		= fun () ->
		let flags = [
		mark "flag" (Some ("type","min")) (Flags.min_to_xml ());
		mark "flag" (Some ("type","proj")) (Flags.proj_to_xml ());
		mark "flag" (Some ("type","join")) (Flags.join_to_xml ());
		mark "flag" (Some ("type","plp")) (Flags.plp_to_xml ());
		mark "flag" (Some ("type","scalar")) (Flags.scalar_to_xml !flag_scalar);
		mark "flag" (Some ("type","lp")) (Flags.lp_to_xml !flag_lp);
		]
		|> String.concat ""
		|> mark "flags" None
		and timings = Timing.to_xml() in
		mark "Lib" (Some ("name","VPL")) (flags ^ timings)

	let apply_timeout : unit -> string
	  = fun () ->
	  let flags = [
	  mark "flag" (Some ("type","min")) (Flags.min_to_xml ());
	  mark "flag" (Some ("type","proj")) (Flags.proj_to_xml ());
	  mark "flag" (Some ("type","join")) (Flags.join_to_xml ());
	  mark "flag" (Some ("type","plp")) (Flags.plp_to_xml ());
	  mark "flag" (Some ("type","scalar")) (Flags.scalar_to_xml !flag_scalar);
	  mark "flag" (Some ("type","lp")) (Flags.lp_to_xml !flag_lp);
	  ]
	  |> String.concat ""
	  |> mark "flags" None
	  and timings = Timing.timeout() in
	  mark "Lib" (Some ("name","VPL")) (flags ^ timings)
	*)
end;;

(* *************************************** *)
(* ************** Execution ************** *)
(* *************************************** *)

let print s =
	if !output_file = None
	then print_endline s
;;

if !debug
then begin
	Vpl.Debug.set_colors();
	Debug.enable();
	Debug.print_enable();
	Profile.enable();
	Profile.reset()
end;;

Profile.reset();
print_endline "Running VPL";
VPL.apply_flags();
try begin
	match !time_budget with
	| None -> ()
	| Some i -> begin
		Pervasives.ignore (Unix.alarm i);
		Sys.set_signal Sys.sigalrm (Sys.Signal_handle timeout_handler)
			end
	end;
	VPL.exec !file;
	Sys.set_signal Sys.sigalrm Sys.Signal_ignore;
	let s = VPL.Timing.to_string () in
	print s;
	(*print_res (VPL.to_xml());*)
	Profile.report()
	|> print;
with
	| Timeout -> begin
	Sys.set_signal Sys.sigalrm Sys.Signal_ignore;
	(*print_res (VPL.apply_timeout ());*)
end;
Profile.disable()
;;
