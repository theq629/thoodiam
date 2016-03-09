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

let process_game_input ch =
	Ui.Key.(
		if ch == int_of_char 'Q' then Some Quit
		else if ch == int_of_char 'g' then Some Pick_up
		else if ch == int_of_char 'i' then Some Inventory
		else if ch == int_of_char 'e' then Some Equipment
		else if ch == int_of_char 'd' then Some Drop
		else if ch == Curses.Key.up || ch == int_of_char 'k' then Some N
		else if ch == Curses.Key.down || ch == int_of_char 'j' then Some S
		else if ch == Curses.Key.left || ch == int_of_char 'h' then Some W
		else if ch == Curses.Key.right || ch == int_of_char 'l' then Some E
		else if ch == int_of_char 'y' then Some NW
		else if ch == int_of_char 'u' then Some NE
		else if ch == int_of_char 'b' then Some SW
		else if ch == int_of_char 'n' then Some SE
		else None
	)

let process_popup_input ch =
	let module Ch_map = Map.Make(struct type t = int;; let compare = compare;; end) in
	let list_id_set, _ =
		Array.fold_left begin fun (m, i) str ->
			Ch_map.add (int_of_char str.[0]) i m, i + 1
		end (Ch_map.empty, 0) Ui.letter_list_ids in
	Ui.Key.(
		if ch == 27 || ch == 10 then Some End
		else if ch == int_of_char '[' || ch == int_of_char ' ' then Some Page_down
		else if ch == int_of_char ']' then Some Page_up
		else begin
			match Ch_map.get ch list_id_set with
			| Some i ->
				Some (List_item i)
			| None ->
				Printf.eprintf "unknown key %i\n" ch;
				None
		end
	)

let do_popup disp extra_styles parent_win ui (f : Disp.Text_view.t -> Ui.Key.t option -> bool) =
	let win = Disp.Window.make disp parent_win (0, 0) (Disp.Window.dim parent_win) in
	let view = Disp.Text_view.make disp win in
	Disp.Text_view.config ~bg_style:extra_styles.Styles.popup_bg view;
	let callback = f view in
	ignore (callback None);
	let rec run () =
		if callback (process_popup_input (Disp.get_key disp)) then begin
			run ()
		end else () in
	run ();
	Disp.Window.remove win

let make_ui ui_styles extra_styles disp =
	let root = Disp.root disp in
	let panel_win, rest_win = Disp.Window.split disp root Disp.Left (13, 13) in
	let status_win, map_win = Disp.Window.split disp rest_win Disp.Bottom (3, 3) in
	let ui = Ui.make
			~panel:(Disp.Text_view.make disp panel_win)
			~status:(Disp.Text_view.make disp status_win)
			~map:(Disp.Chars_view.make disp map_win)
			~styles:ui_styles
			~do_popup:(do_popup disp extra_styles map_win)
			~list_ids:Ui.letter_list_ids
			in
	Disp.Text_view.config ~bg_style:extra_styles.Styles.panel_bg ui.Ui.panel;
	Disp.Text_view.config ~bg_style:extra_styles.Styles.status_bg ui.Ui.status;
	ui

let run map_seed things_seed =
	let game = Thoodiam.init map_seed things_seed in
	Disp.with_display begin fun disp ->
		let ui_styles, extra_styles = make_styles disp in
		let ui = make_ui ui_styles extra_styles disp in
		while match game.Game.player with Some _ -> true | None -> false do
			Ui.draw ui disp game;
			match process_game_input (Disp.get_key disp) with
			| Some k ->
				Ui.handle_input game ui k begin fun cmds ->
					Game.update game cmds;
					Ui.update_game game ui
				end
			| None -> ()
		done
	end

let _ =
	let time = int_of_float (Unix.time ()) in
	let map_seed = ref time in
	let things_seed = ref time in
	Args.(parse [
			"-map-seed", Set_int map_seed,
				"map generation seed";
			"-things-seed", Set_int map_seed,
				"thing generation seed";
		] []);
	run !map_seed !things_seed
