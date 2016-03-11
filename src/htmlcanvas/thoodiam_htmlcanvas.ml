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

let char_int_to_string =
	let special =
		[
			38, "<up>";
			40, "<down>";
			37, "<left>";
			39, "<right>";
			190, ".";
			188, "<";
			219, "[";
			221, "]";
			191, "/";
		] in
	fun ch ->
		let ch = if 65 <= ch && ch <= 90 then ch + 32 else ch in
		try List.assq ch special
		with Not_found -> Key_bindings.char_int_to_string ch

let input_to_string (is_shift, key_code) =
	(if is_shift then "shift+" else "")
	^ (char_int_to_string key_code)

let game_key_bindings =
	Key_bindings.(Ui.Key.(
		let b = make () in
		bind b (false, 38) N;
		bind b (false, 40) S;
		bind b (false, 37) W;
		bind b (false, 39) E;
		bind b (false, 75) N;
		bind b (false, 74) S;
		bind b (false, 72) W;
		bind b (false, 76) E;
		bind b (false, 89) NW;
		bind b (false, 85) NE;
		bind b (false, 66) SW;
		bind b (false, 78) SE;
		bind b (false, 190) Wait;
		bind b (false, 71) Pick_up;
		bind b (false, 73) Inventory;
		bind b (false, 69) Equipment;
		bind b (false, 68) Drop;
		bind b (true, 188) Up_stairs;
		bind b (true, 190) Down_stairs;
		bind b (true, 81) Quit;
		bind b (true, 191) Help;
		b
	))

let popup_key_bindings =
	let module Ch_map = Map.Make(struct type t = int;; let compare = compare;; end) in
	let list_id_set, _ =
		Array.fold_left begin fun (m, i) str ->
			Ch_map.add (int_of_char str.[0]) i m, i + 1
		end (Ch_map.empty, 0) Ui.letter_list_ids in
	Key_bindings.(Ui.Key.(
		let b = make () in
		bind b (false, 27) Finish;
		bind b (false, 13) Finish;
		bind b (false, 219) Page_up;
		bind b (false, 221) Page_down;
		bind b (false, 32) Page_down;
		bind b (false, 89) Yes;
		bind b (false, 78) No;
		other b "[a-zA-Z]" "list item" begin fun (is_shift, key_code) ->
				let use_key_code =
					key_code
					+ if is_shift && key_code >= 65 && key_code <= 90 then 0 else 32 in
				match Ch_map.get use_key_code list_id_set with
				| Some i ->
					Some (List_item i)
				| None ->
					None
			end;
		bind b (true, 191) Help;
		b
	))

let make_ui ui_styles extra_styles disp update_queue =
	let root = Disp.root disp in
	let panel_win, rest_win = Disp.Window.split_fix disp root Disp.Horiz Disp.First (round_to_int (Disp.Text_view.width_with_font Ui.panel_width)) in
	let map_win, status_win = Disp.Window.split_fix disp rest_win Disp.Vert Disp.Second (round_to_int (Disp.Text_view.height_with_font Ui.status_height)) in
	let do_popup ?(show_help=true) ui f =
		let win = Disp.Window.make disp map_win (0, 0) (Disp.Window.dim map_win) in
		let view = Disp.Text_view.make disp win in
		Disp.Text_view.config ~bg:extra_styles.Styles.popup_bg view;
		let update key =
			let ui_key = Opt.flat_map (Key_bindings.get popup_key_bindings) key in
			match ui_key with
			| Some Ui.Key.Help when show_help ->
				Ui.show_key_bindings popup_key_bindings ui
			| _ ->
				if f view ui_key then ()
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
		~do_popup:do_popup
		~input_to_string:input_to_string in
	Disp.Text_view.config ~bg:extra_styles.Styles.panel_bg ui.Ui.panel;
	Disp.Text_view.config ~bg:extra_styles.Styles.status_bg ui.Ui.status;
	Disp.Chars_view.config ~bg:extra_styles.Styles.map_bg ui.Ui.map;
	ui

let update_game disp ui game key =
	Opt.iter begin fun key ->
		Ui.handle_input !game game_key_bindings ui key begin fun cmd ->
			Game.update !game cmd
		end;
		Ui.update_game !game ui
	end (Opt.flat_map (Key_bindings.get game_key_bindings) key);
	Ui.draw ui disp !game

let run map_seed things_seed game_seed skip_welcome =
	let make_game () =
		Thoodiam.init map_seed things_seed game_seed in
	let game = ref (make_game ()) in
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
	let show_welcome () =
		if not skip_welcome then
			Ui.show_info "Thoodiam" Thoodiam_data.welcome_text ui in
	show_welcome ();
	Disp.input_loop disp begin fun key ->
		update (Some key);
		match !game.Game.status with
		| Game.Playing -> ()
		| status ->
			Ui.show_info "Game over" (Thoodiam_data.game_over_text status) ~on_finish:begin fun () ->
				game := make_game ();
				show_welcome ()
			end ui
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
			let skip_welcome = ref false in
			parse_url_params [
					"seed", (fun s -> let s = int_of_string s in map_seed := s; things_seed := s; game_seed := s);
					"mapseed", (fun s -> map_seed := int_of_string s);
					"thingsseed", (fun s -> things_seed := int_of_string s);
					"gameseed", (fun s -> game_seed := int_of_string s);
					"skipwelcome", (fun _ -> skip_welcome := true);
				] (Js.to_string (Dom_html.window##location##href));
			run !map_seed !things_seed !game_seed !skip_welcome
		end
