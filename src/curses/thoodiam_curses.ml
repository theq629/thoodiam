open Std
module Disp = Display_curses
module Ui = Ui.Make(Disp)

module Styles =
	struct
		type style = Disp.Style.t

		type t =
			{
				panel_bg : style;
				status_bg : style;
				popup_bg : style;
				map_bg : style;
			}
	end

let make_styles disp =
	let module C = Disp.Colour in
	let module Cp = Disp.Colour_pair in
	let cols = C.use_defaults disp in
	let black_on_white = Cp.make disp cols.C.black cols.C.white in
	let white_on_black = Cp.make disp cols.C.white cols.C.black in
	let yellow_on_black = Cp.make disp cols.C.yellow cols.C.black in
	let ui_styles =
		Ui.Styles.(Disp.Style.({
			panel_label = make disp ~colours:black_on_white;
			panel_text = make disp ~colours:black_on_white;
			status_text = make disp ~colours:black_on_white;
			popup_label = make disp ~colours:yellow_on_black;
			popup_key = make disp ~colours:yellow_on_black;
			popup_text = make disp ~colours:white_on_black;
			popup_key_sel = make disp ~colours:yellow_on_black;
			popup_text_sel = make disp ~colours:yellow_on_black;
			map_fov = make disp ~colours:yellow_on_black;
			map_seen = make disp ~colours:white_on_black;
		})) in
	let extra_styles =
		Styles.(Disp.Style.({
			panel_bg = make disp ~colours:black_on_white;
			status_bg = make disp ~colours:black_on_white;
			popup_bg = make disp ~colours:white_on_black;
			map_bg = make disp ~colours:white_on_black;
		})) in
	ui_styles, extra_styles

let char_int_to_string =
	let special =
		[
			Curses.Key.up, "<up>";
			Curses.Key.down, "<down>";
			Curses.Key.left, "<left>";
			Curses.Key.right, "<right>";
		] in
	fun ch ->
		try List.assq ch special
		with Not_found -> Key_bindings.char_int_to_string ch

let game_key_bindings =
	Key_bindings.(Ui.Key.(
		let b : (int, Ui.Key.t) Key_bindings.t = make () in
		bind b Curses.Key.up N;
		bind b Curses.Key.down S;
		bind b Curses.Key.left W;
		bind b Curses.Key.right E;
		bind b (int_of_char 'k') N;
		bind b (int_of_char 'j') S;
		bind b (int_of_char 'h') W;
		bind b (int_of_char 'l') E;
		bind b (int_of_char 'y') NW;
		bind b (int_of_char 'u') NE;
		bind b (int_of_char 'b') SW;
		bind b (int_of_char 'n') SE;
		bind b (int_of_char '.') Wait;
		bind b (int_of_char 'g') Pick_up;
		bind b (int_of_char 'i') Inventory;
		bind b (int_of_char 'e') Equipment;
		bind b (int_of_char 'd') Drop;
		bind b (int_of_char '<') Up_stairs;
		bind b (int_of_char '>') Down_stairs;
		bind b (int_of_char 'Q') Quit;
		bind b (int_of_char '?') Help;
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
		bind b 27 Finish;
		bind b 10 Finish;
		bind b (int_of_char '[') Page_up;
		bind b (int_of_char ']') Page_down;
		bind b (int_of_char ' ') Page_down;
		bind b (int_of_char 'y') Yes;
		bind b (int_of_char 'n') No;
		other b "[a-zA-Z]" "list item" begin fun ch ->
				match Ch_map.get ch list_id_set with
				| Some i ->
					Some (List_item i)
				| None ->
					None
			end;
		bind b (int_of_char '?') Help;
		b
	))

let do_popup disp extra_styles parent_win ?(show_help=true) ui f =
	let win = Disp.Window.make disp parent_win (0, 0) (Disp.Window.dim parent_win) in
	let view = Disp.Text_view.make disp win in
	Disp.Text_view.config ~bg_style:extra_styles.Styles.popup_bg view;
	let callback = f view in
	ignore (callback None);
	let rec run () =
		let ui_key = Key_bindings.get popup_key_bindings (Disp.get_key disp) in
		match ui_key with
		| Some Ui.Key.Help when show_help ->
			Ui.show_key_bindings popup_key_bindings ui
		| _ ->
			if callback ui_key then begin
				run ()
			end else () in
	run ();
	Disp.Window.remove win

let make_ui ui_styles extra_styles disp =
	let root = Disp.root disp in
	let panel_win, rest_win = Disp.Window.split disp root Disp.Left (Ui.panel_width, 1) in
	let status_win, map_win = Disp.Window.split disp rest_win Disp.Bottom (1, Ui.status_height) in
	let ui = Ui.make
			~panel:(Disp.Text_view.make disp panel_win)
			~status:(Disp.Text_view.make disp status_win)
			~map:(Disp.Chars_view.make disp map_win)
			~styles:ui_styles
			~do_popup:(do_popup disp extra_styles map_win)
			~list_ids:Ui.letter_list_ids
			~input_to_string:char_int_to_string
			in
	Disp.Text_view.config ~bg_style:extra_styles.Styles.panel_bg ui.Ui.panel;
	Disp.Text_view.config ~bg_style:extra_styles.Styles.status_bg ui.Ui.status;
	ui

let run map_seed things_seed game_seed skip_welcome =
	let game = Thoodiam.init map_seed things_seed game_seed in
	Disp.with_display begin fun disp ->
		let ui_styles, extra_styles = make_styles disp in
		let ui = make_ui ui_styles extra_styles disp in
		if not skip_welcome then
			Ui.show_info "Thoodiam" Thoodiam_data.welcome_text ui;
		while Game.(game.status == Playing) do
			Ui.draw ui disp game;
			match Key_bindings.get game_key_bindings (Disp.get_key disp) with
			| Some k ->
				Ui.handle_input game game_key_bindings ui k begin fun cmd ->
					Game.update game cmd
				end;
				Ui.update_game game ui
			| None -> ()
		done;
		Ui.show_info "Game over" (Thoodiam_data.game_over_text game.Game.status) ui
	end

let _ =
	let time = int_of_float (Unix.time ()) in
	let map_seed = ref time in
	let things_seed = ref time in
	let game_seed = ref time in
	let skip_welcome = ref false in
	Args.(parse [
			"-seed", Int (fun s -> map_seed := s; things_seed := s; game_seed := s),
				"seed for all randomness";
			"-map-seed", Set_int map_seed,
				"map generation seed";
			"-things-seed", Set_int things_seed,
				"thing generation seed";
			"-game-seed", Set_int game_seed,
				"game chances seed";
			"-skip-welcome", Set skip_welcome,
				"skip welcome screen";
		] []);
	run !map_seed !things_seed !game_seed !skip_welcome
