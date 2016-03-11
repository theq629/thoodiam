open Std
module Disp = Display_htmlcanvas
module Ui = Ui.Make(Disp)

module Styles =
	struct
		type col = Disp.Colour.t
		type style = Disp.Style.t

		type t =
			{
				panel_bg : col;
				status_bg : col;
				popup_bg : col;
				map_bg : col;
				death_bg : col;
				death_text : style;
			}
	end

let make_styles disp =
	let c = Disp.Colour.of_string in
	let ui_styles =
		Ui.Styles.(Disp.Style.({
			panel_label = make disp ~fg:(c "black");
			panel_text = make disp ~fg:(c "black");
			status_text = make disp ~fg:(c "black");
			popup_label = make disp ~fg:(c "yellow");
			popup_key = make disp ~fg:(c "yellow");
			popup_text = make disp ~fg:(c "white");
			popup_key_sel = make disp ~fg:(c "yellow");
			popup_text_sel = make disp ~fg:(c "yellow");
			map_fov = make disp ~fg:(c "yellow");
			map_seen = make disp ~fg:(c "white");
		})) in
	let extra_styles =
		Styles.(Disp.Style.({
			panel_bg = c "white";
			status_bg = c "white";
			popup_bg = c "black";
			map_bg = c "black";
			death_bg = c "black";
			death_text = make disp ~fg:(c "red");
		})) in
	ui_styles, extra_styles

let log_unknown_key key =
	let is_shift, key_code = key in
	Printf.eprintf "unknown key %s%i\n" (if is_shift then "shift+" else "") key_code

let process_input key =
	Ui.Key.(
		if key = (true, 81) then Some Quit
		else if key = (false, 190) then Some Wait
		else if key = (false, 38) || key = (false, 75) then Some N
		else if key = (false, 40) || key = (false, 74) then Some S
		else if key = (false, 37) || key = (false, 72) then Some W
		else if key = (false, 39) || key = (false, 76) then Some E
		else if key = (false, 89) then Some NW
		else if key = (false, 85) then Some NE
		else if key = (false, 66) then Some SW
		else if key = (false, 78) then Some SE
		else if key = (false, 71) then Some Pick_up
		else if key = (false, 68) then Some Drop
		else if key = (false, 73) then Some Inventory
		else if key = (false, 69) then Some Equipment
		else begin
			log_unknown_key key;
			None
		end
	)

let process_popup_input key =
	let module Ch_map = Map.Make(struct type t = int;; let compare = compare;; end) in
	let list_id_set, _ =
		Array.fold_left begin fun (m, i) str ->
			Ch_map.add (int_of_char str.[0]) i m, i + 1
		end (Ch_map.empty, 0) Ui.letter_list_ids in
	Ui.Key.(
		if key = (false, 27) || key = (false, 13) then Some End
		else if key = (false, 32) || key = (false, 221) then Some Page_down
		else if key = (false, 219) then Some Page_up
		else begin
			let is_shift, key_code = key in
			let use_key_code =
				key_code
				+ if is_shift && key_code >= 65 && key_code <= 90 then 0 else 32 in
			match Ch_map.get use_key_code list_id_set with
			| Some i ->
				Some (List_item i)
			| None ->
				log_unknown_key key;
				None
		end
	)

let make_ui ui_styles extra_styles disp update_queue =
	let root = Disp.root disp in
	let panel_win, rest_win = Disp.Window.split disp root Disp.Horiz 0.15 in
	let map_win, status_win = Disp.Window.split disp rest_win Disp.Vert 0.8 in
	let do_popup ui f =
		let win = Disp.Window.make disp map_win (0, 0) (Disp.Window.dim map_win) in
		let view = Disp.Text_view.make disp win in
		Disp.Text_view.config ~bg:extra_styles.Styles.popup_bg view;
		let update key =
			if f view (Opt.flat_map process_popup_input key) then ()
			else begin
				update_queue :=
					begin match !update_queue with
					| u::u1::us ->
						u1 None;
						u1::us 
					| [u] -> []
					| [] -> []
					end;
				Disp.Window.remove win
			end in
		update_queue := update :: !update_queue;
		update None in
	let ui = Ui.make
		~panel:(Disp.Text_view.make disp panel_win)
		~status:(Disp.Text_view.make disp status_win)
		~map:(Disp.Chars_view.make disp map_win)
		~styles:ui_styles
		~list_ids:Ui.letter_list_ids
		~do_popup:do_popup in
	Disp.Text_view.config ~bg:extra_styles.Styles.panel_bg ui.Ui.panel;
	Disp.Text_view.config ~bg:extra_styles.Styles.status_bg ui.Ui.status;
	Disp.Chars_view.config ~bg:extra_styles.Styles.map_bg ui.Ui.map;
	ui

let update_game disp ui game key =
	Opt.iter begin fun key ->
		Ui.handle_input game ui key begin fun cmd ->
			Game.update game cmd
		end;
		Ui.update_game game ui
	end (Opt.flat_map process_input key);
	Ui.draw ui disp game

let run map_seed things_seed game_seed =
	let game = Thoodiam.init map_seed things_seed game_seed in
	let disp = Disp.init Dom_html.document##body in
	let ui_styles, extra_styles = make_styles disp in
	let update_queue = ref [] in
	let ui = make_ui ui_styles extra_styles disp update_queue in
	update_queue := (update_game disp ui game) :: !update_queue;
	let update key =
		match !update_queue with
		| [] -> ()
		| u::_ -> u key in
	update None;
	Disp.on_refresh disp begin fun _ ->
		update None
	end;
	Disp.input_loop disp begin fun key ->
		update (Some key)
	end;
	Js._false

let parse_url_params params url =
	let parse_param i (key, on_value) =
		try
			let j =
				max
					(String.find ~start:i ~sub:("?" ^ key) url)
					(String.find ~start:i ~sub:("&" ^ key) url) in
			if j < 0 then raise Not_found
			else
				let k = j + String.length key + 2 in
				let l =
					try String.index_from url k '&'
					with Not_found -> String.length url in
				on_value (String.sub url k (l - k))
		with Not_found | Invalid_argument _ ->
			() in
	try
		let i = String.rindex url '?' in
		List.iter (parse_param i) params
	with Not_found ->
		()

let _ =
	Dom_html.window##onload <- Dom_html.handler begin fun _ ->
			let time = int_of_float ((jsnew Js.date_now ())##getTime ()) in
			let map_seed = ref time in
			let things_seed = ref time in
			let game_seed = ref time in
			parse_url_params [
					"seed", (fun s -> let s = int_of_string s in map_seed := s; things_seed := s; game_seed := s);
					"mapseed", (fun s -> map_seed := int_of_string s);
					"thingsseed", (fun s -> things_seed := int_of_string s);
					"gameseed", (fun s -> game_seed := int_of_string s);
				] (Js.to_string (Dom_html.window##location##href));
			run !map_seed !things_seed !game_seed
		end
