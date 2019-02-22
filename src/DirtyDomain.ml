type state =
    | Top
    | Bot
    | Name of string

module type Type = sig
    include TimedDomain.Type with type t = state
    val assume: string -> Cabs.expression -> t -> t
    val assign : string -> (Domain.variable * Cabs.expression) list -> t -> t
    val meet : string -> t -> t -> t
    val join: string -> t -> t -> t
    val project: string -> Domain.variable list -> t -> t
    val minimize : string -> t -> t
    val widen: string -> t -> t -> t
    val is_bound : string -> bool
end

module Lift (D : TimedDomain.Type) : Type = struct

    (** Map indexed by strings. *)
    module MapS = Map.Make(struct type t = string let compare = Pervasives.compare end)

	(* Map associating abstract values to their name. *)
	let mapVal : D.t MapS.t ref = ref MapS.empty

	(**
		Returns the abstract value currently associated to the given name.
		@raise Invalid_argument if the name has got no association in the map.
	*)
    let get : string -> D.t
        = fun s ->
        try
            MapS.find s !mapVal
        with Not_found -> Pervasives.invalid_arg (Printf.sprintf "Run_Domain.get %s : %s" D.name s)

    let is_bound : string -> bool
        = fun s ->
        MapS.mem s !mapVal

	(** Associates a name and an abstract value in map !{!val:mapVal}. *)
	let set : D.t -> string -> unit
		= fun state name ->
		mapVal := MapS.add name state !mapVal

    (* DOMAIN TYPES *)

    let name = D.name ^ ":dirty"

    type t = state

    let top = Top
    let bottom = Bot

    let state_to_string : t -> string
    	= function
    	| Top -> "top"
    	| Bot -> "bot"
    	| Name name -> name

    let value : t -> D.t
		= function
		| Top -> D.top
		| Bot -> D.bottom
		| Name s -> get s

    let lift : (D.t -> D.t -> D.t) -> string -> t -> t -> t
        = fun operator output_name p1 p2 ->
        set (operator (value p1) (value p2)) output_name;
        Name output_name

    (* OPERATORS *)

    let export_timings = D.export_timings

    let print : t -> unit
        = function
    	| Top -> print_string "top"
    	| Bot -> print_string "bot"
    	| Name name -> begin
            print_endline (name ^ ":");
            D.print (get name)
        end

    let meet = lift D.meet
    let join = lift D.join
    let widen = lift D.widen

	let assume : string -> Cabs.expression -> t -> t
        = fun output_name cond p2 ->
		set (D.assume cond (value p2)) output_name;
        Name output_name

	let assign : string -> (Domain.variable * Cabs.expression) list -> t -> t
        = fun output_name assigns p ->
		set (D.assign assigns (value p)) output_name;
        Name output_name

	let project : string -> Domain.variable list -> t -> t
        = fun output_name vars p ->
		set (D.project vars (value p)) output_name;
        Name output_name

	let minimize : string -> t -> t
        = fun output_name p ->
		set (D.minimize (value p)) output_name;
        Name output_name

    let is_bottom : t -> bool
        = fun p ->
        D.is_bottom (value p)

    let leq : t -> t -> bool
        = fun p1 p2 ->
        D.leq (value p1) (value p2)

    module Interval = D.Interval

    let itvize : Cabs.expression -> t -> Interval.t
        = fun expr p ->
        D.itvize expr (value p)
end
