(**
    This module adds side effects to a timed domain.
    Abstract values are given a name.
*)

type state =
    | Top
    | Bot
    | Name of string

module type Type = sig
    include TimedDomain.Type with type t = state

    (** Computes the effect of a guard on an abstract value. *)
    val assume: string -> Cabs.expression -> t -> t

    val assume_back: string -> Cabs.expression -> t -> t

    (** Computes the effect of a list of parallel assignments on an abstract value. *)
    val assign : string -> (Domain.variable * Cabs.expression) list -> t -> t

    (** Computes the meet of two abstract values. *)
    val meet : string -> t -> t -> t

    (** Computes the join of two abstract values. *)
    val join: string -> t -> t -> t

    (** Eliminates the given list of variables from the given abstract value.*)
    val project: string -> Domain.variable list -> t -> t

    val proj_incl : string -> t -> t -> t option

    (** Minimizes the representation of the given abstract value. *)
    val minimize : string -> t -> t

    (** Computes the widening of two abstract values. *)
    val widen: string -> t -> t -> t

    (** Returns true if the given name is associated to an abstract value. *)
    val is_bound : string -> bool
end

module Lift : functor (D : TimedDomain.Type) -> Type
