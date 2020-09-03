open Vpl;;
open Arg;;


module Prof = Profile.Profile(struct let name = "VPL" end)

(* *************************************** *)
(* ************** Input file ************* *)
(* *************************************** *)


module Cmd = struct

	let file = ref ""

	let output_file = ref None

	let debug = ref false

	let time_budget : int option ref = ref None

	let flag_plp = ref Flags.Adj_Raytracing

	let flag_scalar = ref Flags.Rat

	let flag_proj = ref (fun () -> Flags.FM)

	let flag_join = ref (fun () -> Flags.Baryc)

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

	let spec_list = [
		("-file", String (fun s -> file:=s), Printf.sprintf "<file> input file (default=%s)" !file);
		("-res", String (fun s -> output_file:= Some s),"<file> output file (default=None)");
		("-debug", Unit (fun () -> debug:= true ),"Enable debug mode");
		("-timeout", Int (fun i -> time_budget := Some i ),"Timeout value");
		("-folder", String (fun s -> Interpreter.folder := s), "Folder containing polyhedra files");
		("-plp", String update_plp, "PLP method (raytracing | greedy)");
		("-scalar", String update_scalar, "Scalar type (rat | symb | float)");
		("-proj", String update_proj, "Projection algorithm (fm | plp)");
		("-join", String update_join, "Join algorithm (barycentric | plp | plp_regions)");
		("-projincl", Bool (fun b -> Flags.smart_proj_incl := b), "Smart projinclusion");
	]

	let anon_fun s = failwith ("unsupported argument "^s);;

	let usage_msg = "VPL trace runner. Available options:";;

end

open Cmd;;

Arg.parse spec_list anon_fun usage_msg;;
Flags.log_trace := false;;

if !debug
then begin
	Debugger.set_colors();
    Debugger.enable();
    Debugger.print_enable();
	Profile.enable();
	Profile.reset();
	Min.Debug.disable();
	(*PLP.Debug.enable_all();*)
	PLP.Debug.enable DebugTypes.([Title ; MInput ; MOutput ; Normal; Detail]);
	PSplx.Debug.enable DebugTypes.([Title ; MInput ; MOutput ; Normal; Detail])
end;;

if String.length !file = 0
then failwith "No input file provided";;

Printf.printf "Input file : %s\n" (!file);;

let print_res : string -> unit
	= fun s ->
	match !output_file with
	| None -> ()
	| Some resFile ->
		let chRes = Stdlib.open_out_gen [Open_creat ; Open_wronly ; Open_trunc] 0o640 (resFile) in
		Stdlib.output_string chRes s;
		Stdlib.close_out chRes
;;

(* *************************************** *)
(* *************** Timeout *************** *)
(* *************************************** *)

exception Timeout

let timeout_handler : int -> unit
  =	fun _ -> Stdlib.raise Timeout

(* *************************************** *)
(* *************** Modules *************** *)
(* *************************************** *)

open UserInterface
module I = WrapperTraductors.Interface(Scalar.Rat)
open I

module Ident = Lift_Ident (struct
    type t = string
    let compare = Stdlib.compare
    let to_string s = s
    end)

module Expr = struct
    type t = Cabs.expression

    let rec to_term : t -> Term.t
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
            Ident.addVars [var_name];
            Var (Ident.toVar var_name)
			end
		| _ -> Stdlib.raise Out_of_Scope
		))

    let rec of_term : Term.t -> t
        = Cabs.(Term.(function
        | Cte c -> CONSTANT(CONST_FLOAT (Scalar.Rat.to_string c))
        | Var var -> VARIABLE (Ident.ofVar var)
        | Add(t1, t2) -> BINARY(ADD, of_term t1, of_term t2)
        | Opp t -> UNARY(MINUS, of_term t)
        | Sum [t] -> of_term t
        | Sum (t :: ts) -> BINARY(ADD, of_term t, of_term (Sum ts))
        | Mul(t1, t2) -> BINARY(MUL, of_term t1, of_term t2)
        | Prod [t] -> of_term t
        | Prod (t :: ts) -> BINARY(MUL, of_term t, of_term (Prod ts))
        | _ -> Stdlib.raise Out_of_Scope))
end

module VPL = struct
    module D = struct
        include MakeCustom(Vpl.Domains.UncertifiedQ)(Ident)(Expr)

        module Interval = NCDomain.NCVPL_Unit.I.QInterface.Interval

        let rec expr_to_cond : Cabs.expression -> b_expr
            = function
    		| Cabs.NOTHING -> Basic true
    		| Cabs.UNARY (Cabs.NOT, e) -> Not (expr_to_cond e)
    		| Cabs.BINARY (Cabs.AND, e1, e2)
    		| Cabs.BINARY (Cabs.BAND, e1, e2) -> BinL (expr_to_cond e1, Vpl.WrapperTraductors.AND, expr_to_cond e2)
    		| Cabs.BINARY (Cabs.OR, e1, e2)
    		| Cabs.BINARY (Cabs.BOR, e1, e2) -> BinL (expr_to_cond e1, Vpl.WrapperTraductors.OR, expr_to_cond e2)
    		| Cabs.BINARY (Cabs.EQ, e1, e2) -> Atom (e1, Cstr_type.EQ, e2)
    		| Cabs.BINARY (Cabs.NE, e1, e2) -> Atom (e1, Cstr_type.NEQ, e2)
    		| Cabs.BINARY (Cabs.LE, e1, e2) -> Atom (e1, Cstr_type.LE, e2)
    		| Cabs.BINARY (Cabs.LT, e1, e2) -> Atom (e1, Cstr_type.LT, e2)
    		| Cabs.BINARY (Cabs.GT, e1, e2) -> Atom (e1, Cstr_type.GT, e2)
    		| Cabs.BINARY (Cabs.GE, e1, e2) -> Atom (e1, Cstr_type.GE, e2)
            | _ -> Stdlib.raise Out_of_Scope

        let name = "VPL"

        let print p = to_string var_to_string p
            |> print_endline

        let minimize x = x

        let assume e = assume (expr_to_cond e)

		let assume_back e = assume_back (expr_to_cond e)
    end

    module TimedD = TimedDomain.Lift(D)
    module DirtyD = DirtyDomain.Lift(TimedD)
	include Interpreter.Lift(DirtyD)

	let apply_flags = fun () ->
		begin
			Flags.proj := !flag_proj ();
			Flags.join := !flag_join ();
			Flags.plp := !flag_plp
		end

	let export_timings : unit -> string
		= fun () ->
		Printf.sprintf "%s%s"
			(TimedD.Timing.to_xml ())
			([string_of_int !PLPIncremental.n_new
			|> XMLOutput.mark "new_regions" None;
			string_of_int !PLPIncremental.n_deleted
			|> XMLOutput.mark "deleted_regions" None;
			string_of_int !PLPIncremental.n_updated
			|> XMLOutput.mark "updated_regions" None;
			string_of_int !PLPIncremental.n_total
			|> XMLOutput.mark "total_regions" None;
			]
			|> List.filter ((<>) "")
			|> String.concat ""
			)
		|> XMLOutput.mark "timings" None

	(*
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
		Stdlib.ignore (Unix.alarm i);
		Sys.set_signal Sys.sigalrm (Sys.Signal_handle timeout_handler)
			end
	end;
	Prof.start "exec";
	VPL.exec !file;
	Prof.stop "exec";
	Sys.set_signal Sys.sigalrm Sys.Signal_ignore;
	let s = VPL.export_timings () in
	print s;
	print_res s;
	Profile.report()
	|> print;
with
	| Timeout -> begin
	Sys.set_signal Sys.sigalrm Sys.Signal_ignore;
	(*print_res (VPL.apply_timeout ());*)
end;
Profile.disable()
;;
