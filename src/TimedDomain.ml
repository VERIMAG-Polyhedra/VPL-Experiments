open XMLOutput

module type Type = sig
    include Domain.Type

    module Timing : sig
        val to_xml : unit -> string
    end

    val export_timings : unit -> string
end

module Time = struct
    type t = float

    let zero = 0.
    let add = (+.)
    let sub = (-.)
    let compare = Stdlib.compare
    let toZ x = x *. 10.0**9.0 |> Z.of_float
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
            proj_incl : Z.t;
            assume_back : Z.t;
		}

		let t_ref : t ref = ref {
			join = Z.zero;
      		project = Z.zero;
      		minimize = Z.zero;
			assume = Z.zero;
			assign = Z.zero;
			widen = Z.zero;
            proj_incl = Z.zero;
            assume_back = Z.zero;
        }

		let total : unit -> Z.t
			= fun () ->
			Z.add !t_ref.widen !t_ref.assume
			|> Z.add !t_ref.project
			|> Z.add !t_ref.assign
			|> Z.add !t_ref.join
      		|> Z.add !t_ref.minimize
      		|> Z.add !t_ref.proj_incl
      		|> Z.add !t_ref.assume_back

    	type typ = Assume | Join | Project | Assign | Widen | Minimize | ProjIncl | AssumeBack

		let record : typ -> Time.t -> Time.t -> unit
			= fun typ tbeg tend ->
			let time = Time.sub tend tbeg in
			if Time.compare Time.zero time > 0
			then Stdlib.failwith "negative time"
			else match typ with
				| Assume -> t_ref := {!t_ref with assume = Z.add !t_ref.assume (Time.toZ time)}
                | AssumeBack -> t_ref := {!t_ref with assume_back = Z.add !t_ref.assume_back (Time.toZ time)}
       			| Join -> t_ref := {!t_ref with join = Z.add !t_ref.join (Time.toZ time)}
        		| Minimize -> t_ref := {!t_ref with minimize = Z.add !t_ref.minimize (Time.toZ time)}
				| Project -> begin
                    Printf.sprintf "recording (%f,%f) -> %s for projection"
                        tbeg tend
                        (Time.toZ time |> Z.to_string)
                        |> print_endline;
                    t_ref := {!t_ref with project = Z.add !t_ref.project (Time.toZ time)}
                    end
				| Assign -> t_ref := {!t_ref with assign = Z.add !t_ref.project (Time.toZ time)}
				| Widen -> t_ref := {!t_ref with widen = Z.add !t_ref.project (Time.toZ time)}
                | ProjIncl -> t_ref := {!t_ref with proj_incl = Z.add !t_ref.proj_incl (Time.toZ time)}

		let prTime : Z.t -> string
			= fun t0 ->
			let units = ["ns"; "us"; "ms"; "s" ; "ks" ; "Ms" ; "Bs"] in
			let a = Stdlib.ref 0 in
			let tInt = Stdlib.ref t0 in
			let tDec = Stdlib.ref Z.zero in
			let b = Stdlib.ref true in
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
				  ((if Z.leq !tDec (Z.of_int 100)
                   then "0"
                   else "") ^
                   (Z.div !tDec (Z.of_int 10) |> Z.to_string))
				  (List.nth units !a);
			end

		let to_string : unit -> string
			= fun () ->
			Printf.sprintf "Library %s:\n\twiden : %s\n\tassume : %s\n\tproject : %s\n\tminimize : %s\n\tassign : %s\n\tjoin : %s\n\tproj_incl : %s\n\tassume_back : %s\n\tTOTAL : %s\n"
				D.name
				(prTime !t_ref.widen)
				(prTime !t_ref.assume)
                (prTime !t_ref.project)
                (prTime !t_ref.minimize)
				(prTime !t_ref.assign)
				(prTime !t_ref.join)
				(prTime !t_ref.proj_incl)
				(prTime !t_ref.assume_back)
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
			time_to_xml "proj_incl" !t_ref.proj_incl;
			time_to_xml "assume_back" !t_ref.assume_back;
			time_to_xml "total" (total ())
            ]
			|> List.filter ((<>) "")
			|> String.concat ""

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

    let export_timings _ = mark "timings" None (Timing.to_xml ())

    include D

    let lift1 : ('a -> t) -> Timing.typ -> 'a -> t
        = fun operator typ args -> begin
        let t_beg = Unix.gettimeofday () in
		let res = operator args in
		let t_end = Unix.gettimeofday () in
		Timing.record typ t_beg t_end;
        res
        end

    let lift2 : ('a -> 'b -> t) -> Timing.typ -> 'a -> 'b -> t
        = fun operator typ arg1 arg2 -> begin
        let t_beg = Unix.gettimeofday () in
		let res = operator arg1 arg2 in
		let t_end = Unix.gettimeofday () in
		Timing.record typ t_beg t_end;
        res
        end

    let lift_option2 : ('a -> 'b -> t option) -> Timing.typ -> 'a -> 'b -> t option
        = fun operator typ arg1 arg2 -> begin
        let t_beg = Unix.gettimeofday () in
		let res = operator arg1 arg2 in
		let t_end = Unix.gettimeofday () in
		Timing.record typ t_beg t_end;
        res
        end

    let meet = lift2 meet Timing.Assume

	let assume = lift2 assume Timing.Assume

    let assume_back = lift_option2 assume_back Timing.AssumeBack

	let join = lift2 join Timing.Join

	let widen = lift2 widen Timing.Widen

	let assign = lift2 assign Timing.Assign

	let project = lift2 project Timing.Project

    let proj_incl = lift_option2 proj_incl Timing.ProjIncl

	let minimize = lift1 minimize Timing.Minimize
end
