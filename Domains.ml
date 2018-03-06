open Interpreter;;

module Printer_Domain : Domain.Type = struct

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
