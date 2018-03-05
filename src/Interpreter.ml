(**
	This module provides a functor that takes an abstract domain and allows to run traces with it.
	Each operator has a dedicated timer.
*)

open XMLOutput

(**
	Folder that contains polyhedron files.
	The [load] operator will look for files in this folder.
 *)
let folder : string ref = ref ""

let variables : Domain.variable list ref = ref []

let add_variable : Domain.variable -> unit
	= fun var ->
	variables := !variables @ [var]

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

(**
	This functor takes an abstract domain and provides a function [run] for running the domain on a trace.
*)
module Lift (D : DirtyDomain.Type) = struct

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
			Printf.sprintf "Loaded file %s, obtained %s"
				file (expression_to_string cond)
				|> print_endline;
			cond
		end
(*
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

	let print_last : unit -> unit
		= fun () ->
		let (_,value) = MapS.max_binding !mapVal in
		D.print value

	let exec : string -> unit
		= fun file_name ->
		(* Parsing file *)
		let defs = match Frontc.parse_file file_name Pervasives.stdout with
		| Frontc.PARSING_ERROR ->
		 	Pervasives.failwith "FrontC Parsing Error"
		| Frontc.PARSING_OK defs -> defs
		in
		let main_fun = List.find (function
			| Cabs.FUNDEF((_,_,(name,_,_,_)), body) when String.equal name "main" -> true
			| _ -> false)
			defs
		in
		match main_fun with
		| Cabs.FUNDEF((_,_,(name,_,_,_)), body) ->
			let (mem,stmt) = Stmt.from_body MapS.empty body in
			let _ = run mem stmt in
			()
		| _ -> ()
*)
end
