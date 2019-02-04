(**
    This modules provides a type for abstract domains.
	Operators rely on the expression type {!type:Cabs.expression}, which is an AST for C expressions.
*)

(** Type for variables during the parsing process. *)
type variable = string

(** Type of abstract domains. *)
module type Type = sig

	(** Module defining an interval datatype, see {!val:itvize}. *)
	module Interval : sig
		type t
	end

    (** Name of the domain. *)
	val name : string

	(** Type of an abstract value. *)
	type t

	(** Top abstract value *)
	val top: t

	(** Bottom abstract value *)
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
