(**
    This module adds timing measurements to each operator of a domain.
*)

(** Type of timed domains. *)
module type Type = sig
    include Domain.Type

    module Timing : sig
        val to_xml : unit -> string
    end

    val export_timings : unit -> string
end

(** Timing module. *)
module Time : sig
    type t

    val zero : t
    val add : t -> t -> t
    val sub : t -> t -> t
    val compare : t -> t -> int
    val toZ : t -> Z.t
end

(** Functor that lifts a domain into a timed one. *)
module Lift : functor (D : Domain.Type) -> Type
