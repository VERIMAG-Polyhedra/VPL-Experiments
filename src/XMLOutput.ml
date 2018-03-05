let rec(range : int -> int -> int list)
	= fun i j->
	if i >= j then [] else i :: (range (i+1) j)

let add_tab : int -> string -> string
	= fun n s ->
	let str_tab = String.make n '\t' in
	List.fold_left
		(fun res i -> let c = String.get s i in
			if c = '\n'
			then res ^ "\n" ^ str_tab
			else res ^ (String.make 1 c))
		str_tab
		(range 0 (String.length s))

let mark : string -> (string * string) option -> string -> string
	= fun head opt content ->
	match opt with
	| None ->
		Printf.sprintf "<%s>%s</%s>"
			head (add_tab 1 content) head
	| Some (name, value) ->
		Printf.sprintf "<%s %s=\"%s\">%s</%s>"
			head name value (add_tab 1 content) head
