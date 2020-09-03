exception Out_of_Scope

module Translate = struct

    module E = Apron.Texpr1
    module C = Apron.Coeff

    module Set = Set.Make (struct type t = Apron.Var.t let compare = Apron.Var.compare end)

	(* TODO : est-ce ce qu'on veut? *)
	let rnd = E.Rnd
	let typ = E.Real

    let var : string -> Apron.Var.t
		= Apron.Var.of_string

    (**
     * Translates a FrontC arithmetic expression into an Apron one.
     *)
    let rec arith_expr : Cabs.expression -> E.expr * Set.t
        = Cabs.(function
        | VARIABLE var_name ->
            let v = Apron.Var.of_string var_name in
            (E.Var v, Set.add v Set.empty)
		| CONSTANT (CONST_INT c) -> E.Cst (int_of_string c |> C.s_of_int), Set.empty
		| CONSTANT (CONST_FLOAT f) -> E.Cst (float_of_string f |> C.s_of_float), Set.empty
		| UNARY (MINUS, e) ->
            let (e, vs) = arith_expr e in
            (E.Unop (E.Neg, e, typ, rnd), vs)
		| UNARY (PLUS, e) -> arith_expr e
		| UNARY (POSINCR, e) -> arith_expr (BINARY (ADD, e, CONSTANT (CONST_INT "1")))
		| UNARY (POSDECR, e) -> arith_expr (BINARY (SUB, e, CONSTANT (CONST_INT "1")))
		| BINARY (ADD, e1, e2) ->
            let (e1', vs1) = arith_expr e1
            and (e2', vs2) = arith_expr e2 in
            let vs = Set.union vs1 vs2 in
            (E.Binop (E.Add, e1', e2', typ, rnd), vs)
		| BINARY (MUL, e1, e2) ->
            let (e1', vs1) = arith_expr e1
            and (e2', vs2) = arith_expr e2 in
            let vs = Set.union vs1 vs2 in
            (E.Binop (E.Mul, e1', e2', typ, rnd), vs)
		| BINARY (SUB, e1, e2) ->
            let (e1', vs1) = arith_expr e1
            and (e2', vs2) = arith_expr e2 in
            let vs = Set.union vs1 vs2 in
            (E.Binop (E.Sub, e1', e2', typ, rnd), vs)
		| _ -> Stdlib.raise Out_of_Scope
		)

    let add_vars : Apron.Environment.t -> Apron.Var.t list -> Apron.Environment.t
		= fun env l ->
		List.fold_left
			(fun env v ->
				try Apron.Environment.add env [||] [| v |]
				with Failure _ -> env)
			env l

    (**
     * Translates a FrontC condition (of the form [e1 binop e2]) into an Apron constraint.
     *)
    let cstr : Apron.Environment.t -> Cabs.expression * Cabs.binary_operator * Cabs.expression -> Apron.Tcons1.t * Apron.Environment.t
        = fun env (e1, binop, e2) ->
        let t = Cabs.BINARY (Cabs.SUB, e1, e2) in
        let (e, vs) = arith_expr t in
        let env = add_vars env (Set.elements vs) in
        let expr = E.of_expr env e in
        let cons = (match binop with
            | Cabs.LE ->
    			let expr' = E.unop E.Neg expr typ rnd in
    		 	Apron.Tcons1.make expr' Apron.Tcons1.SUPEQ
    		| Cabs.LT ->
    			let expr' = E.unop E.Neg expr typ rnd in
    		 	Apron.Tcons1.make expr' Apron.Tcons1.SUP
    		| Cabs.GE -> Apron.Tcons1.make expr Apron.Tcons1.SUPEQ
    		| Cabs.GT -> Apron.Tcons1.make expr Apron.Tcons1.SUP
    		| Cabs.EQ -> Apron.Tcons1.make expr Apron.Tcons1.EQ
    		| Cabs.NE -> Apron.Tcons1.make expr Apron.Tcons1.DISEQ
            | _ -> Stdlib.raise Out_of_Scope)
        in
        (cons, env)
end

open Apron

module Make (Man: sig
	type t
	val name : string
	val manager: t Manager.t
	end) = struct

    let name = Man.name

    type t = Man.t Abstract1.t

    let man = Man.manager

	let empty_env = Environment.make [||] [||]

	let top = Abstract1.top man empty_env

    let bottom = Abstract1.bottom man empty_env

    let is_bottom = Abstract1.is_bottom man

    let update_env : Environment.t -> t -> t
		= fun env state ->
		if Environment.compare env (Abstract1.env state) = 0
		then state
		else Abstract1.change_environment Man.manager state env false (* TODO: que fait ce booléen?*)

    let update_states state1 state2 =
		let env1 = Abstract1.env state1
		and env2 = Abstract1.env state2 in
		if Environment.equal env1 env2
		then (state1,state2)
		else let env' = Environment.lce (Abstract1.env state1) (Abstract1.env state2) in
			(update_env env' state1,
			 update_env env' state2)

    let leq = fun state1 state2 ->
		let (state1',state2') = update_states state1 state2 in
		Abstract1.is_leq man state1' state2'

    let join : t -> t -> t
		= fun state1 state2 ->
		let (state1',state2') = update_states state1 state2 in
		Abstract1.join man state1' state2'

    let widen = Abstract1.widening man

	let meet = Abstract1.meet man

    let assume_cstr : Cabs.expression * Cabs.binary_operator * Cabs.expression -> t -> t
        = fun (e1, op, e2) state ->
        let env = Abstract1.env state in
        let (cons, env') = Translate.cstr env (e1, op, e2) in
        let earray = ref (Tcons1.array_make env' 1) in
        let cons' = Tcons1.extend_environment cons env' in
        Tcons1.array_set !earray 0 cons';
        let state' = update_env env' state in
        Abstract1.meet_tcons_array man state' !earray

    let rec assume: Cabs.expression -> t -> t
        = fun expr state ->
        Cabs.(match expr with
        | BINARY (op, e1, e2) when op = EQ || op = NE || op = LT || op = GT || op = LE || op = GE -> assume_cstr (e1, op, e2) state
        | BINARY (AND, e1, e2) | BINARY (BAND, e1, e2) ->
            assume e1 state
            |> assume e2
        | BINARY (OR, e1, e2) | BINARY (BOR, e1, e2)  ->
            join (assume e1 state) (assume e2 state)
        | _ -> Stdlib.raise Out_of_Scope
        )

    let assign : (Domain.variable * Cabs.expression) list -> t -> t
		= fun l state ->
		let env = Abstract1.env state in
		let (assigns,env) = List.fold_right
			(fun (v,t) (assigns,env) ->
				let (e,vs) = Translate.arith_expr t in
				let v' = Translate.var v in
				let vs = Translate.Set.add v' vs |> Translate.Set.elements in
				let env' = Translate.add_vars env vs
					|> Environment.lce env in
				(v', Texpr1.of_expr env' e) :: assigns, env'
			)
			l ([],env)
		in
		let state = update_env env state in
		List.fold_left
			(fun state (var, expr) -> Abstract1.assign_texpr man state var expr None)
			state assigns

    let project : Domain.variable list -> t -> t
		= fun l state ->
		let vars = List.map Translate.var l
		|> List.filter (Environment.mem_var (Abstract1.env state))
		|> Array.of_list
		in
		let abs_proj = Abstract1.forget_array man state vars true in
        Abstract1.forget_array man abs_proj vars false
        (* boolean = true -> projection ; boolean = false -> forget *)

    let minimize : t -> t
        = fun state ->
        Abstract1.minimize man state;
        state

    module Interval = Interval

    let itvize : Cabs.expression -> t -> Interval.t
		= fun t state ->
        let (e, vs) = Translate.arith_expr t in
		let env = Translate.add_vars (Abstract1.env state) (Translate.Set.elements vs) in
		let state = update_env env state in
		Texpr1.of_expr env e
		|> Abstract1.bound_texpr man state

    let print : t -> unit
		= fun state ->
		Abstract1.print Format.std_formatter state

    let proj_incl : t -> t -> t option
        = fun p1 p2 ->
        let (_,env1) = Abstract1.env p1 |> Environment.vars
        and (_,env2) = Abstract1.env p2 |> Environment.vars in
        let vars_to_project = Array.fold_left (fun acc var ->
            if Array.mem var env2
            then acc
            else var::acc
        ) [] env1
        |> List.map Apron.Var.to_string
        in
        Printf.sprintf "Proj_incl: Eliminating variables %s"
            (String.concat " ; " vars_to_project)
            |> print_endline;
        let p' = if vars_to_project = []
            then p1
            else project vars_to_project p1
        in
        if leq p2 p'
        then Some p'
        else None

    let assume_back _ _ = failwith "assumeback"
end

module Polka_Loose = Make (struct
    let name = "Polka_strict"
    type t = Polka.strict Polka.t
	let manager = Polka.manager_alloc_strict ()
    end)

module RUN = struct
    module D = Polka_Loose
    module TimedD = TimedDomain.Lift(D)
    module DirtyD = DirtyDomain.Lift(TimedD)
    include Interpreter.Lift(DirtyD)
end

open Arg;;

(* *************************************** *)
(* ************** Input file ************* *)
(* *************************************** *)


module Cmd = struct

	let file = ref ""

	let output_file = ref None

	let debug = ref false

	let time_budget : int option ref = ref None

	let spec_list = [
		("-file", String (fun s -> file:=s), Printf.sprintf "<file> input file (default=%s)" !file);
		("-res", String (fun s -> output_file:= Some s),"<file> output file (default=None)");
		("-debug", Unit (fun () -> debug:= true ),"Enable debug mode");
		("-timeout", Int (fun i -> time_budget := Some i ),"Timeout value");
		("-folder", String (fun s -> Interpreter.folder := s), "Folder containing polyhedra files");
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
		let chRes = Stdlib.open_out_gen [Open_creat ; Open_wronly ; Open_append] 0o640 (resFile) in
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
(* ************** Execution ************** *)
(* *************************************** *)

let print s =
	if !output_file = None
	then print_endline s
;;

print_endline "Running Apron";
try begin
	match !time_budget with
	| None -> ()
	| Some i -> begin
		Stdlib.ignore (Unix.alarm i);
		Sys.set_signal Sys.sigalrm (Sys.Signal_handle timeout_handler)
			end
	end;
	RUN.exec !file;
	Sys.set_signal Sys.sigalrm Sys.Signal_ignore;
with
	| Timeout -> begin
	Sys.set_signal Sys.sigalrm Sys.Signal_ignore;
	(*print_res (VPL.apply_timeout ());*)
end;;

exit 0;;
