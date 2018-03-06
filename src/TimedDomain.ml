open XMLOutput

module type Type = sig
    include Domain.Type

    val export_timings : unit -> string
end

module Lift (D : Domain.Type) : Type = struct

	(**
		Defines a timer for some operators.
	*)
	module Timing = struct
		type t = {
    		join : Z.t;
        	project : Z.t;
        	minimize : Z.t;
    		assume : Z.t;
    		assign : Z.t;
    		widen : Z.t;
		}

		let t_ref : t ref = ref {
			join = Z.zero;
      		project = Z.zero;
      		minimize = Z.zero;
			assume = Z.zero;
			assign = Z.zero;
			widen = Z.zero;
        }

		let total : unit -> Z.t
			= fun () ->
			Z.add !t_ref.widen !t_ref.assume
			|> Z.add !t_ref.project
			|> Z.add !t_ref.assign
			|> Z.add !t_ref.join
      		|> Z.add !t_ref.minimize

    	type typ = Assume | Join | Project | Assign | Widen | Minimize

		let record : typ -> int64 -> int64 -> unit
			= fun typ tbeg tend ->
			let time = Int64.sub tend tbeg in
			if Int64.compare Int64.zero time > 0
			then Pervasives.failwith "negative time"
			else match typ with
				| Assume -> t_ref := {!t_ref with assume = Z.add !t_ref.assume (Z.of_int64 time)}
       			| Join -> t_ref := {!t_ref with join = Z.add !t_ref.join (Z.of_int64 time)}
        		| Minimize -> t_ref := {!t_ref with minimize = Z.add !t_ref.minimize (Z.of_int64 time)}
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
			Printf.sprintf "Library %s:\n\twiden : %s\n\tassume : %s\n\tproject : %s\n\tminimize : %s\n\tassign : %s\n\tjoin : %s\n\tTOTAL : %s\n"
				D.name
				(prTime !t_ref.widen)
				(prTime !t_ref.assume)
                (prTime !t_ref.project)
                (prTime !t_ref.minimize)
				(prTime !t_ref.assign)
				(prTime !t_ref.join)
				(prTime (total ()))

	    let prRealTime : Z.t -> string
	      = fun t ->
	      Z.to_string t

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

    let export_timings = Timing.to_xml

    include D

    let lift : ('a -> 'b) -> Timing.typ -> 'a -> 'b
        = fun operator typ args ->
        let t_beg = Oclock.gettime Oclock.realtime in
		let res = operator args in
		let t_end = Oclock.gettime Oclock.realtime in
		Timing.record typ t_beg t_end;
        res

    let meet = lift meet Timing.Assume

	let assume = lift assume Timing.Assume

	let join = lift join Timing.Join

	let widen = lift widen Timing.Widen

	let assign = lift assign Timing.Assign

	let project = lift project Timing.Project

	let minimize = lift minimize Timing.Minimize

end
