open Trace_reader;;

module Printer_Domain = struct

	module D = struct

		module Interval = struct
			type t = unit
		end

		let name = "Printer Domain"

		type t = unit

		let top = ()
		let bottom = ()
		let is_bottom _ = false

		let assume e _ = Printf.sprintf "assume %s"
			(expression_to_string e)
			|> print_endline

		let assign l _ = Printf.sprintf "assign %s"
			(Vpl.Misc.list_to_string
				(fun (var, e) -> Printf.sprintf "%s <- %s"
					var
					(expression_to_string e))
				l " ; ")
			|> print_endline

		let meet _ _ = print_endline "meet"

		let join _ _ = print_endline "join"

		let project vars _ = Printf.sprintf "project %s"
			(Vpl.Misc.list_to_string (fun x -> x) vars " ; ")
			|> print_endline

		let minimize _ = print_endline "minimize"

		let widen _ _ = print_endline "widen"

		let leq _ _ = print_endline "leq" ; true

		let print _ = print_endline "print"

		let itvize _ _ = print_endline "itvize"
	end

	include Run_Domain(D)
end

module VPL = struct

	open Vpl

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
		| CONSTANT (CONST_FLOAT f) -> Cte (Scalar.Rat.of_string f)
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
end
