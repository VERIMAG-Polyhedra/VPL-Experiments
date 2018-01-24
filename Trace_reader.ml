(**
	This module provides a functor that takes an abstract domain and allows to run traces with it.
	Each operator has a dedicated timer.
*)

open IOBuild

(** Type for variables during the parsing process. *)
type variable = string

let variables : variable list ref = ref []

let add_variable : variable -> unit
	= fun var ->
	variables := !variables @ [var]

(** Type for abstract states. *)
type state =
	| Top
	| Bot
	| Name of string

let state_to_string : state -> string
	= function
	| Top -> "top"
	| Bot -> "bot"
	| Name name -> name

(**
	The type for an abstract domain module.
	Operators rely on the expression type {!type:Cabs.expression}, which is an AST for C expressions.
*)
module type Domain = sig

	(** Module defining an interval datatype, see {!val:itvize}. *)
	module Interval : sig
		type t
	end

	val name : string

	(** Type of an abstract value. *)
	type t

	(** Top abstract value *)
	val top: t

	(** Bot abstract value *)
	val bottom: t

	(** Tests if the given abstract value is bottom. *)
	val is_bottom: t -> bool

	(** Computes the effect of a guard on an abstract value. *)
	val assume: Cabs.expression -> t -> t

	(** Computes the effect of a list of parallel assignments on an abstract value. *)
	val assign : (variable * Cabs.expression) list -> t -> t

	(** Computes the meet of two abstract values. *)
	val meet : t -> t -> t

	(** Computes the join of two abstract values. *)
	val join: t -> t -> t

	(** Eliminates the given list of variables from the given abstract value.*)
	val project: variable list -> t -> t

	(** Minimizes the representation of the given abstract value. *)
  	val minimize : t -> t

	(** Computes the widening of two abstract values. *)
	val widen: t -> t -> t

	(** [leq a1 a2] tests if [a1] is included into [a2]. *)
	val leq: t -> t -> bool

	val print : t -> unit

	(** Computes an interval of the values that the given expression can reach in the given abstract value. *)
	val itvize : t ->  Cabs.expression -> Interval.t
end

(**
	Folder that contains polyhedron files.
	The [load] operator will look for files in this folder.
 *)
let folder : string ref = ref ""

let (list_to_string : ('a -> string) -> 'a list -> string -> string)
	= fun to_string l sep->
	Printf.sprintf "[%s]" (String.concat sep (List.map to_string l))

let rec expression_to_string : Cabs.expression -> string
	= Cabs.(function
	| NOTHING -> "skip"
	| UNARY (MINUS, e) -> Printf.sprintf "-(%s)" (expression_to_string e)
	| UNARY (PLUS, e) -> Printf.sprintf "+(%s)" (expression_to_string e)
	| UNARY (NOT, e) -> Printf.sprintf "!(%s)" (expression_to_string e)
	| UNARY (POSINCR, e) -> Printf.sprintf "%s++" (expression_to_string e)
	| UNARY (POSDECR, e) -> Printf.sprintf "%s--" (expression_to_string e)
	| BINARY (ADD, e1, e2) -> Printf.sprintf "%s + %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (SUB, e1, e2) -> Printf.sprintf "%s - %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (MUL, e1, e2) -> Printf.sprintf "%s * %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (DIV, e1, e2) -> Printf.sprintf "%s / %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (AND, e1, e2) -> Printf.sprintf "%s && %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (OR, e1, e2) -> Printf.sprintf "%s || %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (BAND, e1, e2) -> Printf.sprintf "%s & %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (BOR, e1, e2) -> Printf.sprintf "%s | %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (EQ, e1, e2) -> Printf.sprintf "%s == %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (LE, e1, e2) -> Printf.sprintf "%s <= %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (LT, e1, e2) -> Printf.sprintf "%s < %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (GE, e1, e2) -> Printf.sprintf "%s >= %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (GT, e1, e2) -> Printf.sprintf "%s > %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (NE, e1, e2) -> Printf.sprintf "%s != %s" (expression_to_string e1) (expression_to_string e2)
	| BINARY (ASSIGN, e1, e2) -> Printf.sprintf "%s = %s" (expression_to_string e1) (expression_to_string e2)
	| CALL (f, args) -> Printf.sprintf "%s (%s)"
		(expression_to_string f)
		(list_to_string expression_to_string args ", ")
	| COMMA es -> list_to_string expression_to_string es ", "
	| CONSTANT (CONST_INT i) -> i
	| CONSTANT (CONST_FLOAT f) -> f
	| VARIABLE var -> var
	| _ -> Pervasives.invalid_arg "expression_to_string"
	)

(* For XML *)
let add_tab : int -> string -> string
	= fun n s ->
	let str_tab = String.make n '\t' in
	List.fold_left
		(fun res i -> let c = String.get s i in
			if c = '\n'
			then res ^ "\n" ^ str_tab
			else res ^ (String.make 1 c))
		str_tab
		(range 0 (String.length s))

let mark : string -> (string * string) option -> string -> string
	= fun head opt content ->
	match opt with
	| None ->
		Printf.sprintf "<%s>%s</%s>"
			head (add_tab 1 content) head
	| Some (name, value) ->
		Printf.sprintf "<%s %s=\"%s\">%s</%s>"
			head name value (add_tab 1 content) head

(**
	This functor takes an abstract domain and provides a function [run] for running the domain on a trace.
*)
module Run_Domain (D : Domain) = struct

	(**
		Defines a timer for some operators.
	*)
	module Timing = struct
		type t = {
		join : Z.t;
		minkowski : Z.t;
    	project : Z.t;
    	minimize : Z.t;
		assume : Z.t;
		assign : Z.t;
		widen : Z.t;
		}

		let t_ref : t ref = ref {
			join = Z.zero;
			minkowski = Z.zero;
      		project = Z.zero;
      		minimize = Z.zero;
			assume = Z.zero;
			assign = Z.zero;
			widen = Z.zero;}

		let total : unit -> Z.t
			= fun () ->
			Z.add !t_ref.widen !t_ref.assume
			|> Z.add !t_ref.project
			|> Z.add !t_ref.assign
			|> Z.add !t_ref.join
			|> Z.add !t_ref.minkowski
      		|> Z.add !t_ref.minimize

    	type typ = Assume | Join | Minkowski | Project | Assign | Widen | Minimize

		let record : typ -> int64 -> int64 -> unit
			= fun typ tbeg tend ->
			let time = Int64.sub tend tbeg in
			if Int64.compare Int64.zero time > 0
			then Pervasives.failwith "negative time"
			else match typ with
				| Assume -> t_ref := {!t_ref with assume = Z.add !t_ref.assume (Z.of_int64 time)}
       			| Join -> t_ref := {!t_ref with join = Z.add !t_ref.join (Z.of_int64 time)}
        		| Minimize -> t_ref := {!t_ref with minimize = Z.add !t_ref.minimize (Z.of_int64 time)}
				| Minkowski -> t_ref := {!t_ref with minkowski = Z.add !t_ref.minkowski (Z.of_int64 time)}
				| Project -> t_ref := {!t_ref with project = Z.add !t_ref.project (Z.of_int64 time)}
				| Assign -> t_ref := {!t_ref with assign = Z.add !t_ref.project (Z.of_int64 time)}
				| Widen -> t_ref := {!t_ref with widen = Z.add !t_ref.project (Z.of_int64 time)}

		let prTime : Z.t -> string
			= fun t0 ->
			let units = ["ns"; "us"; "ms"; "s" ; "ks" ; "Ms" ; "Bs"] in
			let a = Pervasives.ref 0 in
			let tInt = Pervasives.ref t0 in
			let tDec = Pervasives.ref Z.zero in
			let b = Pervasives.ref true in
			begin
				while !b && !a < List.length units
				do
				  let (tInt', tDec') = Z.div_rem !tInt (Z.of_int 1000) in
				  if Z.equal Z.zero tInt'
				  then b := false
				  else
					 begin
						tInt := tInt';
						tDec := tDec';
						a := !a + 1;
					 end
				done;
				Printf.sprintf
				  "%s.%s%s"
				  (Z.to_string !tInt)
				  (Z.div !tDec (Z.of_int 10) |> Z.to_string)
				  (List.nth units !a);
			end

		let to_string : unit -> string
			= fun () ->
			Printf.sprintf "Library %s:\n\twiden : %s\n\tassume : %s\n\tproject : %s\n\tminimize : %s\n\tassign : %s\n\tjoin : %s\n\tminkowski : %s\n\tTOTAL : %s\n"
				D.name
				(prTime !t_ref.widen)
				(prTime !t_ref.assume)
        (prTime !t_ref.project)
        (prTime !t_ref.minimize)
				(prTime !t_ref.assign)
				(prTime !t_ref.join)
				(prTime !t_ref.minkowski)
				(prTime (total ()))

	    let prRealTime : Z.t -> string
	      = fun t ->
	      Z.to_string t

	    let to_res : unit -> string
	      	= fun () ->
	      	Printf.sprintf "[\"%s\", %s, %s, %s, %s, %s, %s, %s, %s, \"%s\"],"
	        D.name
	        (prRealTime !t_ref.widen)
	        (prRealTime !t_ref.assume)
	        (prRealTime !t_ref.project)
	        (prRealTime !t_ref.minimize)
	        (prRealTime !t_ref.assign)
	        (prRealTime !t_ref.join)
	        (prRealTime !t_ref.minkowski)
	        (prRealTime (total ()))
	        (prTime (total ()))

		let time_to_xml : string -> Z.t -> string
		    = fun s t ->
			if Z.equal t Z.zero
			then ""
			else Printf.sprintf "%s%s"
				(mark s (Some ("unit","ns")) (Z.to_string t))
				(mark s (Some ("unit","readable")) (prTime t))

		let to_xml : unit -> string
		    = fun () -> [
			time_to_xml "widen" !t_ref.widen;
			time_to_xml "assume" !t_ref.assume;
			time_to_xml "project" !t_ref.project;
			time_to_xml "min" !t_ref.minimize;
			time_to_xml "assign" !t_ref.assign;
			time_to_xml "join" !t_ref.join;
			time_to_xml "minkowski" !t_ref.minkowski;
			time_to_xml "total" (total ())]
			|> List.filter ((<>) "")
			|> String.concat ""
			|> mark "timings" None

		let timeout' : string -> string
			= fun s ->
			mark s (Some ("unit","ns")) "timeout"

		let timeout : unit -> string
		    = fun () ->
			List.map timeout' [
			"widen";
			"assume";
			"project";
			"min";
			"assign";
			"join";
			"minkowski";
			"total"]
			|> List.filter ((<>) "")
			|> String.concat ""
			|> mark "timings" None
	end

	(** Map indexed by strings. *)
	module MapS = Map.Make(struct type t = string let compare = Pervasives.compare end)

	(* Map associating abstract values to their name. *)
	let mapVal : D.t MapS.t ref = ref MapS.empty

	(**
		Returns the abstract value currently assoaciated to the given name.
		@raise Invalid_argument if the name has got no association in the map.
	*)
	let get : string -> D.t
		= fun s ->
		try
			MapS.find s !mapVal
		with Not_found -> Pervasives.invalid_arg (Printf.sprintf "Run_Domain.get %s : %s" D.name s)

	(** Associates a name and an abstract value in map !{!val:mapVal}. *)
	let set : D.t -> string -> unit
		= fun state name ->
		mapVal := MapS.add name state !mapVal

	let value : state -> D.t
		= function
		| Top -> D.top
		| Bot -> D.bottom
		| Name s -> get s

	(**
		Each operator that has a timer is redined here.
	*)
	module Timed_Operators = struct

		let meet p1 p2 p3 =
			let t_beg = Oclock.gettime Oclock.realtime in
			set (D.meet (value p2) (value p3)) p1;
			let t_end = Oclock.gettime Oclock.realtime in
			Timing.record Timing.Assume t_beg t_end

		let guard p1 p2 cond =
			let t_beg = Oclock.gettime Oclock.realtime in
			set (D.assume cond (value p2)) p1;
			let t_end = Oclock.gettime Oclock.realtime in
			Timing.record Timing.Assume t_beg t_end

		let join p1 p2 p3 =
			let t_beg = Oclock.gettime Oclock.realtime in
			set (D.join (value p2) (value p3)) p1;
			let t_end = Oclock.gettime Oclock.realtime in
			Timing.record Timing.Join t_beg t_end

		let widen p1 p2 p3 =
			let t_beg = Oclock.gettime Oclock.realtime in
			set (D.widen (value p2) (value p3)) p1;
			let t_end = Oclock.gettime Oclock.realtime in
			Timing.record Timing.Widen t_beg t_end

		let assign p1 p2 assigns =
			let t_beg = Oclock.gettime Oclock.realtime in
			set (D.assign assigns (value p2)) p1;
			let t_end = Oclock.gettime Oclock.realtime in
			Timing.record Timing.Assign t_beg t_end

		let project p1 p2 vars =
			let t_beg = Oclock.gettime Oclock.realtime in
			set (D.project vars (value p2)) p1;
			let t_end = Oclock.gettime Oclock.realtime in
      		Timing.record Timing.Project t_beg t_end

		let minimize p1 p2 =
			let t_beg = Oclock.gettime Oclock.realtime in
			set (D.minimize (value p2)) p1;
			let t_end = Oclock.gettime Oclock.realtime in
			Timing.record Timing.Minimize t_beg t_end
	end

	let load : string -> Cabs.expression
		= let rec(substring : string -> int -> string)
			= fun s i ->
			(String.sub s i ((String.length s) - i))
		in
		let is_the_poly : string -> string -> bool
			= fun id s ->
			try
				let ri = String.rindex s '_' in
				let id_s = substring s (ri+1) in
				String.equal id id_s
			with Not_found | Invalid_argument _ -> false
		in
		(* Replaces variable names in the condition by that of [variables] in the right order. *)
		let rec replace_variables : Cabs.expression -> Cabs.expression
			= Cabs.(function
			| CONSTANT _ as c-> c
			| VARIABLE v -> let id = IOBuild.get_var_id v in
				if id < List.length !variables
				then VARIABLE (List.nth !variables id)
				else VARIABLE v
			| UNARY (op, e) -> UNARY (op, replace_variables e)
			| BINARY (op, e1, e2) -> BINARY (op, replace_variables e1, replace_variables e2)
			| _ -> Pervasives.invalid_arg "replace_variables"
			)
		in
		fun file_name ->
		Printf.sprintf "Loading file %s from folder %s"
			file_name !folder
			|> print_endline ;
		let ri = String.rindex file_name '.' in
		let id = substring file_name (ri+1) in
		let name = String.sub file_name 0 ri in
		let file = Printf.sprintf "%s/%s.vpl" !folder name in
		let in_ch = Pervasives.open_in file in
		let poly = ref []
		and register = ref false in
		(* Skip polyhedron name *)
		try
			while true do
				let s = Pervasives.input_line in_ch in
				if is_the_poly id s
				then begin
					poly := [file_name];
					register := true;
				end
				else if !register
					then if String.length s > 0 && String.get s 0 = 'P'
						then Pervasives.raise End_of_file
						else if not(String.equal s "")
							then poly := s :: !poly
			done;
			Pervasives.failwith "Trace_reader.load"
		with End_of_file -> begin
			let s = List.rev !poly
				|> String.concat "\n" in
			Printf.sprintf "Parsing matrix %s" s
				|> print_endline ;
			let cond = FCParser.one_matrix FCLexer.token (Lexing.from_string (s ^ "\n"))
				|> IOBuild.to_cond
				|> replace_variables
			in
			Printf.sprintf "Loaded file %s" file
				|> print_endline;
			cond
		end

	(**
		This modules parses C traces and performs the calls to the abstract domain.
		The C parsing is done thanks to the FrontC library.
	*)
	module Stmt = struct

		type t =
			| Skip
			| IsBottom of state
			| Load of string * string
			| Meet of string * state * state (* p1 := p2 && p3 *)
			| Guard of string * state * Cabs.expression (* p1 := p2 && cond *)
			| Join of string * state * state (* p1 := p2 || p3 *)
			(*| Minkowski of name * state * (Q.t * Pol.Var.t * Q.t) list (* p1 := p2 ⊕ [x1, -1, 1 ; ... ; xn, -1, 1] *)*)
			| Widen of string * state * state (* p1 := p2 widen p3 *)
			| Assign of string * state * (variable * Cabs.expression) list (* p1 := [v1 := 3, v3 := v2 + 2] in p3 *)
			| Project of string * state * variable list (* p1 := p2 | [v1, v2] *)
			| Minimize of string * state (* p1 := min p2 *)
			| Includes of state * state (* p1 includes p2 *)
			| Itv of state * Cabs.expression
			| GetLowerBound of state * Cabs.expression
			| GetUpperBound of state * Cabs.expression
			| Sequence of t * t
			| If of Cabs.expression * t * t (* if(cond){then-stmt}else{else-stmt} *)
			| While of Cabs.expression * t
			| CAssign of variable * Cabs.expression (* A C assignment*)

		module Value = struct
			type t =
				| Int of int option
				| Float of float option

			let to_string : t -> string
				= function
				| Int (Some i) -> Printf.sprintf "int %i" i
				| Int None -> "int"
				| Float (Some f) -> Printf.sprintf "float %f" f
				| Float None -> "float"

			let op : (int -> int -> int) -> (float -> float -> float) -> t -> t -> t =
			fun int_op float_op v1 v2 ->
			match v1,v2 with
			| Int (Some i1), Int (Some i2) -> Int (Some (int_op i1 i2))
			| Float (Some f1), Float (Some f2) -> Float (Some (float_op f1 f2))
			| Int (Some i1), Float (Some f2) -> Float (Some (float_op (float_of_int i1) f2))
			| Float (Some f1), Int (Some i2) -> Float (Some (float_op f1 (float_of_int i2)))
			| _-> Pervasives.failwith "Value.op: unexpected None value"

			let add = op (+) (+.)

			let sub = op (-) (-.)

			let mul = op ( * ) ( *. )

			let div = op (/) (/.)

			let opp x = op (fun i j -> -1 * i) (fun u v -> -1. *. u) x (Int (Some 0))

			let bop : (int -> int -> 'bool) -> (float -> float -> 'bool) -> t -> t -> bool
				= fun int_op float_op v1 v2 ->
				match v1,v2 with
				| Int (Some i1), Int (Some i2) -> int_op i1 i2
				| Float (Some f1), Float (Some f2) -> float_op f1 f2
				| Int (Some i1), Float (Some f2) -> float_op (float_of_int i1) f2
				| Float (Some f1), Int (Some i2) -> float_op f1 (float_of_int i2)
				| _-> Pervasives.failwith "Value.bop: unexpected None value"

			let le = bop (<=) (<=)
			let lt = bop (<) (<)
			let ge = bop (>=) (>=)
			let gt = bop (>) (>)
			let eq = bop (=) (=)
			let neq = bop (<>) (<>)
		end

		type mem = Value.t MapS.t

		let rec is_state : Cabs.expression -> bool
			= Cabs.(function
			| CALL (VARIABLE fun_name, [])
				when String.equal fun_name "bot" || String.equal fun_name "top" ->
				true
			| CALL (VARIABLE fun_name, [s1;s2]) when is_state s1 && is_state s2 -> begin
				match fun_name with
				| "meet" | "widen" | "join" -> true
				| _ -> false
				end
			| CALL (VARIABLE "guard", [s1;e]) when is_state s1 -> true
			| VARIABLE name -> begin
				try get name ; true
				with Invalid_argument _  -> false
				end
			| e -> false
			)

		let rec parse_state : Cabs.expression -> state
			= Cabs.(function
			| CALL (VARIABLE fun_name, []) when String.equal fun_name "top" -> Top
			| CALL (VARIABLE fun_name, []) when String.equal fun_name "bot" -> Bot
			| CALL(VARIABLE "load", [CONSTANT (CONST_STRING file_name)]) ->
				let cond = load file_name in
				Timed_Operators.guard "VPL_RESERVED" Top cond;
				(Name "VPL_RESERVED")
			| CALL(VARIABLE "project", args)
				when List.length args > 0 && is_state (List.hd args)
				&& List.for_all (function VARIABLE _ -> true | _ -> false) (List.tl args) ->
				let vars = List.map
					(function VARIABLE var -> var | _ -> invalid_arg "from_body")
					(List.tl args)
				in
				Timed_Operators.project "VPL_RESERVED" (parse_state (List.hd args)) vars;
				(Name "VPL_RESERVED")
			| CALL (VARIABLE fun_name, [s1;s2]) when is_state s1 && is_state s2 -> begin
				match fun_name with
				| "meet" -> Timed_Operators.meet "VPL_RESERVED" (parse_state s1) (parse_state s2);
					(Name "VPL_RESERVED")
				| "widen" -> Timed_Operators.widen "VPL_RESERVED" (parse_state s1) (parse_state s2);
					(Name "VPL_RESERVED")
				| "join" -> Timed_Operators.join "VPL_RESERVED" (parse_state s1) (parse_state s2);
					(Name "VPL_RESERVED")
				| _ -> Pervasives.invalid_arg "parse_state"
				end
			| CALL (VARIABLE "guard", [s1;e]) when is_state s1 ->
				Timed_Operators.guard "VPL_RESERVED" (parse_state s1) e;
				(Name "VPL_RESERVED")
			| VARIABLE name -> Name name
			| _ -> Pervasives.invalid_arg "parse_state"
			)

		let parse_assign : Cabs.expression -> (variable * Cabs.expression)
			= Cabs.(function
			| BINARY (ASSIGN, (VARIABLE var), e) -> (var, e)
			| _ -> Pervasives.failwith "Unexpected assignment"
			)

		let is_computation : Cabs.expression -> bool
			= Cabs.(function
			| BINARY (ASSIGN, VARIABLE var, e) -> true
			| UNARY (POSINCR, VARIABLE var) -> true
			| UNARY (POSDECR, VARIABLE var) -> true
			| _ -> false
			)

		let parse_computation : Cabs.expression -> variable * Cabs.expression
			= Cabs.(function
			| BINARY (ASSIGN, VARIABLE var, e) -> (var, e)
			| UNARY (POSINCR, VARIABLE var) -> (var, BINARY (ADD, VARIABLE var, CONSTANT (CONST_INT "1")))
			| UNARY (POSDECR, VARIABLE var) -> (var, BINARY (SUB, VARIABLE var, CONSTANT (CONST_INT "1")))
			| _ -> Pervasives.invalid_arg "parse_computation"
			)

		let rec eval_aexpr : mem -> Cabs.expression -> Value.t
			= fun mem -> Cabs.(function
			| CONSTANT (CONST_INT c) -> Value.Int (Some (int_of_string c))
			| CONSTANT (CONST_FLOAT c) -> Value.Float (Some (float_of_string c))
			| VARIABLE name -> begin
				try
					match MapS.find name mem with
					| Value.Int (Some i) -> Value.Float (Some (float_of_int i))
					| Value.Float (Some f) -> Value.Float (Some f)
					| Value.Int None | Value.Float None -> Pervasives.failwith ("Variable " ^ name ^ " is used but not initialized")
				with Not_found ->
					Pervasives.failwith ("Variable " ^ name ^ " is not declared")
				end
			| UNARY (PLUS, e) -> eval_aexpr mem e
			| UNARY (MINUS, e) -> Value.opp (eval_aexpr mem e)
			| UNARY (POSINCR, e) -> Value.add (eval_aexpr mem e) (Value.Int (Some 1))
			| UNARY (POSDECR, e) -> Value.sub (eval_aexpr mem e) (Value.Int (Some 1))
			| BINARY (ADD, e1, e2)-> Value.add (eval_aexpr mem e1) (eval_aexpr mem e2)
			| BINARY (SUB, e1, e2)-> Value.sub (eval_aexpr mem e1) (eval_aexpr mem e2)
			| BINARY (MUL, e1, e2)-> Value.mul (eval_aexpr mem e1) (eval_aexpr mem e2)
			| BINARY (DIV, e1, e2)-> Value.div (eval_aexpr mem e1) (eval_aexpr mem e2)
			| NOTHING -> Value.Int None
			(*| CALL(e, arguments) -> *)
			| e -> begin
				Cprint.print_expression e 5;
				Value.Int (Some 0)
				end
			)

		let rec eval_bexpr : mem -> Cabs.expression -> bool
			= fun mem -> Cabs.(function
			| UNARY (NOT, e) -> not (eval_bexpr mem e)
			| UNARY (_, _) -> Pervasives.failwith "eval_bexpr: Unexpected unary expression"
			| BINARY (AND, e1, e2) -> (eval_bexpr mem e1) && (eval_bexpr mem e2)
			| BINARY (BAND, e1, e2) -> (eval_bexpr mem e1) & (eval_bexpr mem e2)
			| BINARY (OR, e1, e2) -> (eval_bexpr mem e1) || (eval_bexpr mem e2)
			| BINARY (BOR, e1, e2) -> (eval_bexpr mem e1) or (eval_bexpr mem e2)
			| BINARY (LE, e1, e2) -> Value.le (eval_aexpr mem e1) (eval_aexpr mem e2)
			| BINARY (LT, e1, e2) -> Value.lt (eval_aexpr mem e1) (eval_aexpr mem e2)
			| BINARY (GE, e1, e2) -> Value.ge (eval_aexpr mem e1) (eval_aexpr mem e2)
			| BINARY (GT, e1, e2) -> Value.gt (eval_aexpr mem e1) (eval_aexpr mem e2)
			| BINARY (EQ, e1, e2) -> Value.eq (eval_aexpr mem e1) (eval_aexpr mem e2)
			| BINARY (NE, e1, e2) -> Value.neq (eval_aexpr mem e1) (eval_aexpr mem e2)
			| CALL (VARIABLE function_name, [e1 ; e2]) when is_state e1 && is_state e2 ->
				D.leq (parse_state e2 |> value) (parse_state e1 |> value)
			| _ -> Pervasives.failwith "Unexpected boolean expression"
			)
		let update_mem : mem -> variable -> Cabs.expression -> mem
			= fun mem var e ->
			MapS.add var (eval_aexpr mem e) mem

		(** Initializes the memory with the type of variables *)
		let init_variables : mem -> Cabs.definition list -> mem
			= fun mem defs ->
			List.fold_left
				(fun mem -> function
					| Cabs.DECDEF(Cabs.INT (_,_), _, names) ->
						List.fold_left
							(fun mem (name,_,_,e) ->
							match eval_aexpr mem e with
							| Value.Int i -> MapS.add name (Value.Int i) mem
							| Value.Float _ -> Pervasives.failwith ("Variable " ^ name ^ " is declared as int but is given a float value")
							)
							mem names
					| Cabs.DECDEF(Cabs.FLOAT _, _, names) ->
						List.fold_left
							(fun mem (name,_,_,e) ->
							match eval_aexpr mem e with
							| Value.Int (Some i) -> MapS.add name (Value.Float (Some (float_of_int i))) mem
							| Value.Int None -> MapS.add name (Value.Float None) mem
							| Value.Float f -> MapS.add name (Value.Float f) mem
							)
							mem names
					| Cabs.DECDEF(Cabs.NAMED_TYPE "abs_value", _, names) -> begin
							List.iter (fun (name, _, _, _) -> set D.top name) names;
							mem
						end
					| Cabs.DECDEF(Cabs.NAMED_TYPE "var", _, names) -> begin
							List.iter (fun (name, _, _, _) -> add_variable name) names;
							mem
						end
					| _ -> mem
				)
				mem defs

		let rec from_body : mem -> Cabs.body -> mem * t
			= fun mem (defs, stmt) ->
			let mem = init_variables mem defs in
			Cabs.(match stmt with
			| NOP -> (mem, Skip)
			| COMPUTATION (BINARY (ASSIGN, (VARIABLE res_name), CALL(VARIABLE fun_name, [st1; st2])))
				when is_state st1 && is_state st2 -> begin
				match fun_name with
				| "meet" -> (mem, Meet (res_name, parse_state st1, parse_state st2))
				| "join" -> (mem, Join (res_name, parse_state st1, parse_state st2))
				| "widen" -> (mem, Widen (res_name, parse_state st1, parse_state st2))
				| _ -> Pervasives.failwith "Unexpected function call with two abstract states"
				end
			| COMPUTATION (BINARY (ASSIGN, (VARIABLE res_name), CALL(VARIABLE "load", [CONSTANT (CONST_STRING file_name)]))) ->
				(mem, Load (res_name, file_name))
			| COMPUTATION (BINARY (ASSIGN, (VARIABLE res_name), CALL(VARIABLE "project", args)))
				when List.length args > 0 && is_state (List.hd args)
				&& List.for_all (function VARIABLE _ -> true | _ -> false) (List.tl args) ->
				let vars = List.map
					(function VARIABLE var -> var | _ -> invalid_arg "from_body")
					(List.tl args)
				in
				(mem, Project (res_name, parse_state (List.hd args), vars))
			| COMPUTATION (BINARY (ASSIGN, (VARIABLE res_name), CALL(VARIABLE fun_name, [st; e])))
				when is_state st && not (is_state e) -> begin
				match fun_name with
				| "guard" -> (mem, Guard (res_name, parse_state st, e))
				| "assign" -> (mem, Assign(res_name, parse_state st, [parse_assign e]))
				| _ -> Pervasives.failwith "Unexpected function call with one abstract state"
				end
			| COMPUTATION e when is_computation e ->
				let (var, assign) = parse_computation e in
				(mem, CAssign (var, assign))
			| IF (e, s1, s2) ->
				let (mem', s1') = from_body mem ([], s1) in
				let (mem'', s2') = from_body mem' ([], s2) in
				(mem'', If (e, s1', s2'))
			| WHILE (e, s) ->
				let (mem',s') = from_body mem ([], s) in
				(mem', While (e, s'))
			| BLOCK body -> from_body mem body
			| SEQUENCE (s1,s2) ->
				let (mem', s1') = from_body mem ([], s1) in
				let (mem'', s2') = from_body mem' ([], s2) in
				(mem'', Sequence (s1', s2'))
			| _ -> begin
				Cprint.print_statement stmt;
				Pervasives.failwith "Unexpected statement"
				end
			)

		let statement_to_string : t -> string =
			let rec(string_repeat : string -> int -> string)
				= fun s i ->
				if i = 0 then "" else String.concat "" [s ; string_repeat s (i-1)]
			in
			let rec statement_to_string_rec : int -> t -> string
				= fun level ->
				function
				| Skip -> Printf.sprintf "%sskip"
					(string_repeat "\t" level)
				| IsBottom s ->
					Printf.sprintf "%sisBottom(%s)"
						(string_repeat "\t" level)
						(state_to_string s)
				| Load(name, file_name) ->
					Printf.sprintf "%s%s = load(%s)"
					(string_repeat "\t" level)
					name file_name
				| Meet (name, s1, s2) ->
					Printf.sprintf "%s%s = meet(%s, %s)"
					(string_repeat "\t" level)
					name (state_to_string s1) (state_to_string s2)
				| Guard (name, s, e) ->
					Printf.sprintf "%s%s = guard(%s, %s)"
					(string_repeat "\t" level)
					name (state_to_string s)
					(expression_to_string e)
				| Sequence (s1, s2) ->
					Printf.sprintf "%s;\n%s"
						(statement_to_string_rec level s1 )
						(statement_to_string_rec level s2)
				| If (e, s1, s2) ->
					Printf.sprintf "%sif (%s){\n%s\n%s}\n%selse{\n%s\n%s}"
						(string_repeat "\t" level)
						(expression_to_string e)
						(statement_to_string_rec (level + 1) s1)
						(string_repeat "\t" level)
						(string_repeat "\t" level)
						(statement_to_string_rec (level + 1) s2)
						(string_repeat "\t" level)
				| While (e, s) ->
					Printf.sprintf "%swhile(%s){\n%s\n%s}"
					(string_repeat "\t" level)
					(expression_to_string e)
					(statement_to_string_rec (level + 1) s)
					(string_repeat "\t" level)
				| CAssign (var, e) ->
					Printf.sprintf "%s%s = %s"
					(string_repeat "\t" level)
					var
					(expression_to_string e)
				| _ -> ""
			in
			fun stmt ->
			statement_to_string_rec 0 stmt

	end

	let rec run : Stmt.mem -> Stmt.t -> Stmt.mem
		= fun mem -> Stmt.(function
		| IsBottom p -> begin
			let _ = D.is_bottom (value p) in ()
			end;
			mem
		| Includes (p1,p2) -> begin
			let _ = D.leq (value p2) (value p1) in ()
			end;
			mem
		| Load (p,s) ->
			let cond = load s in
			Timed_Operators.guard p Top cond;
			mem
		| Meet (p1,p2,p3) ->
			Timed_Operators.meet p1 p2 p3;
			mem
		| Guard (p1,p2,cond) ->
			Timed_Operators.guard p1 p2 cond;
			mem
		| Join (p1,p2,p3) ->
			Timed_Operators.join p1 p2 p3;
			mem
		| Widen (p1,p2,p3) ->
			Timed_Operators.widen p1 p2 p3;
			mem
		| Assign(p1, p2, assigns) ->
			Timed_Operators.assign p1 p2 assigns;
			mem
		| Project (p1,p2,vars) ->
			Timed_Operators.project p1 p2 vars;
			mem
	    | Minimize (p1,p2) ->
			Timed_Operators.minimize p1 p2;
			mem
		| Itv (p, t) -> begin
			let _ = D.itvize (value p) t in ()
			end;
			mem
		| Sequence(s1, s2) ->
			let mem' = run mem s1 in
			run mem' s2
		| If (e, s1, s2) ->
			if eval_bexpr mem e
			then run mem s1
			else run mem s2
		| While (e, s) ->
			if eval_bexpr mem e
			then begin
				let mem' = run mem s in
				run mem' (While (e, s))
			end
			else mem
		| CAssign (var, e) ->
			update_mem mem var e
		| Skip ->
			mem
		| _ -> Pervasives.invalid_arg (Printf.sprintf "Run_Domain.stmt %s" D.name))

	exception End_of_line

	let print_last : unit -> unit
		= fun () ->
		let (_,value) = MapS.max_binding !mapVal in
		D.print value
end
