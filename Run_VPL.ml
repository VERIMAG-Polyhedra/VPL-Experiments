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
		("-folder", String (fun s -> Interpreter.folder := s), "Folder containing polyhedra files");
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

if !debug
then
    if Mpi.comm_rank Mpi.comm_world = 0
    then begin
    	Debug.set_colors();
        Debug.enable();
        Debug.print_enable();
        Debug.enable_one (module PLPDistrib.DebugMaster);
    	Profile.enable();
    	Profile.reset()
    end
    else begin
        Debug.set_colors();
        Debug.print_enable();
        (*PLPDistrib.DebugSlave.enable DebugTypes.([Title ; MInput ; MOutput ; Normal ; Detail]);
        Debug.enable_one (module PLPCore.Debug)*)
    end
;;

module PLP = PLP.PLP(Min.Classic(Vector.Rat.Positive));;
if Mpi.comm_rank Mpi.comm_world > 0
then begin
    (* Since slaves start with a random point: *)
    Random.init (Sys.time() *. 100000. |> int_of_float);
    PLP.Distributed.Slave.first_whip ()
end;;

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

	include WrapperTraductors.Interface(Scalar.Rat)

	exception Out_of_Scope

	module M = Map.Make(struct type t = string let compare = Pervasives.compare end)

	(* Map associating variable names (as string) to their VPL representation. *)
	let mapVar : Var.Positive.t M.t ref = ref M.empty
	let next : Var.Positive.t ref = ref Var.Positive.u

    let map_to_string : unit -> string
        = fun () ->
        Misc.list_to_string
            (fun (name,var) -> Printf.sprintf "%s -> %s" name (Var.Positive.to_string var))
            (M.bindings !mapVar) ";"

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
		| CONSTANT (CONST_FLOAT f) -> Cte (float_of_string f |> Scalar.Rat.of_float)
		| VARIABLE var_name -> begin
			if M.mem var_name !mapVar
			then Var (M.find var_name !mapVar)
			else begin
				let value = !next in
				mapVar := M.add var_name !next !mapVar;
				next := Var.Positive.next !next;
				Var value
			end
			end
		| _ -> Pervasives.raise Out_of_Scope
		))

	let rec to_cond : Cabs.expression -> Cond.t
		= function
		| Cabs.NOTHING -> Cond.Basic true
		| Cabs.UNARY (Cabs.NOT, e) -> Cond.Not (to_cond e)
		| Cabs.BINARY (Cabs.AND, e1, e2)
		| Cabs.BINARY (Cabs.BAND, e1, e2) -> Cond.BinL (to_cond e1, Vpl.WrapperTraductors.AND, to_cond e2)
		| Cabs.BINARY (Cabs.OR, e1, e2)
		| Cabs.BINARY (Cabs.BOR, e1, e2) -> Cond.BinL (to_cond e1, Vpl.WrapperTraductors.OR, to_cond e2)
		| Cabs.BINARY (Cabs.EQ, e1, e2) -> Cond.Atom (to_term e1, Vpl.Cstr.EQ, to_term e2)
		| Cabs.BINARY (Cabs.NE, e1, e2) -> Cond.Atom (to_term e1, Vpl.Cstr.NEQ, to_term e2)
		| Cabs.BINARY (Cabs.LE, e1, e2) -> Cond.Atom (to_term e1, Vpl.Cstr.LE, to_term e2)
		| Cabs.BINARY (Cabs.LT, e1, e2) -> Cond.Atom (to_term e1, Vpl.Cstr.LT, to_term e2)
		| Cabs.BINARY (Cabs.GT, e1, e2) -> Cond.Atom (to_term e1, Vpl.Cstr.GT, to_term e2)
		| Cabs.BINARY (Cabs.GE, e1, e2) -> Cond.Atom (to_term e1, Vpl.Cstr.GE, to_term e2)
		| _ -> Pervasives.raise Out_of_Scope

	module D = struct

		include NCDomain.NCVPL_Cstr.Q

		module Interval = NCDomain.NCVPL_Unit.I.QInterface.Interval

		let name = "VPL"

		let print p = to_string Pol.Var.to_string p
			|> print_endline

		let minimize x = x

		let assume e = assume (to_cond e)

		let assign l = assign (List.map
			(fun (var, e) -> (M.find var !mapVar, to_term e))
			l)

		let project vars = projectM (List.map
			(fun var -> print_endline (map_to_string ()); M.find var !mapVar)
			vars)

		let itvize state e = itvize state (to_term e)

	end

    module TimedD = TimedDomain.Lift(D)
    module DirtyD = DirtyDomain.Lift(TimedD)
	include Interpreter.Lift(DirtyD)

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
	let s = VPL.export_timings () in
	print s;
	Profile.report()
	|> print;
with
	| Timeout -> begin
	Sys.set_signal Sys.sigalrm Sys.Signal_ignore;
	(*print_res (VPL.apply_timeout ());*)
end;
Profile.disable()
;;
