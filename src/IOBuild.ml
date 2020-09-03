type cmp = Le | Ge | Eq | Neq | Lt | Gt

type t = {
	dim : int;
	coeffs : (Q.t list * cmp) list
}

let to_string : t -> string
	= fun d ->
	Printf.sprintf "m %i\n%s\n"
		d.dim
		(List.map
			(fun (l,_) -> List.map Q.to_string l |> String.concat " ")
		     d.coeffs
		|> String.concat "\n")

let isValid : t -> bool
	= fun d ->
	List.for_all (fun (l,_) -> List.length l = d.dim + 1) d.coeffs

let mkRaw : int -> (Q.t list * cmp) list -> t
	= fun d c -> {dim = d; coeffs = c}

let dim : (Q.t list * cmp) list -> int
	= fun c ->
	List.nth c 0 |> fst |> List.length |> (+) (-1)

let mk : (Q.t list * cmp) list -> t
	= fun c ->
	let dim = dim c in
	let r = mkRaw dim c in
	if isValid r then r
	else invalid_arg "IOBuild.mk"

let cmp_to_cabs : cmp -> Cabs.binary_operator
	= Cabs.(function
	| Le -> LE
	| Ge -> GE
	| Eq -> EQ
	| Neq -> NE
	| Lt  -> LT
	| Gt -> GT
	)

let rec(range : int -> int -> int list)
	= fun i j->
	if i >= j then [] else i :: (range (i+1) j)

(** sublist l i j returns l[i:j] (j exluded) *)
let rec(sublist : 'a list -> int -> int -> 'a list)
	= fun l i j->
	match (l,i,j) with
	| ([],_,_) -> []
	| (_,_,0) -> []
	| (p :: tail,_,_) -> if i > 0
	then sublist tail (i-1) (j-1)
	else p :: (sublist tail i (j-1))

let get_vars : int -> string list
	= fun dim ->
	List.map
		(Printf.sprintf "VAR%i")
		(range 0 dim)

let q_to_constant : Q.t -> Cabs.expression
	= fun q ->
	let f = (float_of_string (q.Q.num |> Z.to_string)) /. (float_of_string (q.Q.den |> Z.to_string)) in
	Cabs.CONSTANT (Cabs.CONST_FLOAT (string_of_float f))

let cstr_to_cond : int -> Q.t list * cmp -> Cabs.expression
	= fun dim (coeffs, cmp_op) ->
	let lin = List.fold_left2
		(fun cstr coeff var ->
			let prod = Cabs.BINARY (Cabs.MUL, q_to_constant coeff, (Cabs.VARIABLE var)) in
			Cabs.BINARY (Cabs.ADD, cstr, prod)
			)
		(Cabs.CONSTANT (Cabs.CONST_INT "0"))
		(sublist coeffs 0 dim)
		(get_vars dim)
	and cste = q_to_constant (List.nth coeffs dim)
	and op = cmp_to_cabs cmp_op in
	Cabs.BINARY (op, lin, cste)

let to_cond : t -> Cabs.expression
	= fun d ->
	List.fold_left
		(fun cond coeffs ->
			Cabs.BINARY (Cabs.AND, cond, cstr_to_cond d.dim coeffs))
		(cstr_to_cond d.dim (List.hd d.coeffs))
		(List.tl d.coeffs)

let get_var_id : string -> int
	= fun s ->
	String.sub s 3 ((String.length s)-3)
	|> int_of_string
