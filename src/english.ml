let int num =
	let suffix =
		match num mod 10 with
		| 1 -> "st"
		| 2 -> "nd"
		| 3 -> "rd"
		| _ -> "th" in
	(string_of_int num) ^ suffix

let strings_list list =
	let vowels = "aeiouAEIOU" in
	let add_article str =
		match str with
		| "" -> ""
		| s ->
			try
				ignore (String.index vowels s.[0]);
				"an " ^ s
			with Not_found ->
				"a " ^ s in
	match list with
	| [] -> "nothing"
	| [s] -> add_article s
	| [s1; s2] ->
		String.concat " and " [add_article s1; add_article s2]
	| _ ->
		let num = List.length list in
		String.concat ", " (List.mapi (fun i s -> (if i < num - 1 then "" else "and ") ^ add_article s) list)

let strings_list_bare sep nothing list =
	match list with
	| [] -> nothing
	| [s] -> s
	| [s1; s2] ->
		String.concat (" " ^ sep ^ " ") [s1; s2]
	| _ ->
		let num = List.length list in
		String.concat ", " (List.mapi (fun i s -> (if i < num - 1 then "" else sep ^ " ") ^ s) list)
