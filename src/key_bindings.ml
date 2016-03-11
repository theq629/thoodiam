let special_char_names =
	[|
		"null";
		"start of heading";
		"start of text";
		"end of text";
		"end of transmission";
		"enquiry";
		"acknowledge";
		"bell";
		"backspace";
		"tab";
		"enter";
		"vertical tab";
		"form feed";
		"carriage return";
		"shift out";
		"shift in";
		"data link escape";
		"device control 1";
		"device control 2";
		"device control 3";
		"device control 4";
		"negative acknowledge";
		"synchronous idle";
		"end of trans. block";
		"cancel";
		"end of medium";
		"substitute";
		"escape";
		"file separator";
		"group separator";
		"record separator";
		"unit separator";
		"space"
	|]

let char_int_to_string ch =
	if ch < Array.length special_char_names then Printf.sprintf "<%s>" special_char_names.(ch)
	else if ch > 20 && ch <= 126 then String.make 1 (char_of_int ch)
	else Printf.sprintf "<%i>" ch

let char_to_string char =
	let n = int_of_char char in
	if n < Array.length special_char_names then Printf.sprintf "<%s>" special_char_names.(n)
	else if n > 20 && n <= 126 then String.make 1 char
	else Printf.sprintf "<%i>" n

type ('a, 'b) t =
	{
		mutable list : 'a list;
		table : ('a, 'b) Hashtbl.t;
		mutable other : (string * string * ('a -> 'b option)) option;
	}

let make () =
	{
		list = [];
		table = Hashtbl.create 10;
		other = None;
	}

let bind bnds inp outp =
	if not (Hashtbl.mem bnds.table inp) then begin
		bnds.list <- inp::bnds.list;
		Hashtbl.add bnds.table inp outp
	end

let other bnds inp_str outp_str f =
	bnds.other <- Some (inp_str, outp_str, f)

let iter f bnds =
	List.iter begin fun inp ->
		f inp (Hashtbl.find bnds.table)
	end bnds.list

let get bnds inp =
	try Some (Hashtbl.find bnds.table inp)
	with Not_found ->
		match bnds.other with
		| None -> None
		| Some (_, _, f) -> f inp

let to_list bnds =
	List.map begin fun inp ->
		(inp, Hashtbl.find bnds.table inp)
	end bnds.list

let to_strings_list inp_to_string outp_to_string bnds =
	let strs =
		List.map begin fun inp ->
			(inp_to_string inp, outp_to_string (Hashtbl.find bnds.table inp))
		end (List.rev bnds.list) in
	match bnds.other with
	| None -> strs
	| Some (inp_str, outp_str, _) -> strs @ [inp_str, outp_str]
