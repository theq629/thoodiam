include Containers
module Opt = CCOpt

let round_to_int x = int_of_float (0.5 +. x)

let remove_from_list xs x =
	let found = ref false in
	let xs1 = List.filter begin fun y ->
			if x = y then begin
				found := true;
				false
			end else
				true
		end xs in
	(* TODO: faster? *)
	!found, xs1

let rec list_index x =
	let rec run i =
		function
		| [] -> None
		| y::ys when y = x -> Some i
		| _::ys -> run (i + 1) ys in
	run 0
