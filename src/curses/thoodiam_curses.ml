module Disp = Display_curses
module Ui = Ui.Make(Disp)

module Styles =
	struct
		type style = Disp.Style.t

		type t =
			{
				panel_bg : style;
				status_bg : style;
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
			panel_text = make disp ~colours:black_on_white;
			status_text = make disp ~colours:black_on_white;
			map_fov = make disp ~colours:yellow_on_black;
			map_seen = make disp ~colours:white_on_black;
		})) in
	let extra_styles =
		Styles.(Disp.Style.({
			panel_bg = make disp ~colours:black_on_white;
			status_bg = make disp ~colours:black_on_white;
			map_bg = make disp ~colours:white_on_black;
		})) in
	ui_styles, extra_styles

let make_ui styles disp =
	let root = Disp.root disp in
	let panel_win, rest_win = Disp.Window.split disp root Disp.Left (10, 10) in
	let status_win, map_win = Disp.Window.split disp rest_win Disp.Bottom (3, 3) in
	let ui = Ui.({
			panel = Disp.Text_view.make disp panel_win;
			map = Disp.Chars_view.make disp map_win;
			status = Disp.Text_view.make disp status_win;
		}) in
	Disp.Text_view.config ~bg_style:styles.Styles.panel_bg ui.Ui.panel;
	Disp.Text_view.config ~bg_style:styles.Styles.status_bg ui.Ui.status;
	ui

let process_input ch =
	if ch == int_of_char 'Q' then Some Game.Quit
	else if ch == Curses.Key.up || ch == int_of_char 'k' then Some Game.(Player_move N)
	else if ch == Curses.Key.down || ch == int_of_char 'j' then Some Game.(Player_move S)
	else if ch == Curses.Key.left || ch == int_of_char 'h' then Some Game.(Player_move W)
	else if ch == Curses.Key.right || ch == int_of_char 'l' then Some Game.(Player_move E)
	else if ch == int_of_char 'y' then Some Game.(Player_move NW)
	else if ch == int_of_char 'u' then Some Game.(Player_move NE)
	else if ch == int_of_char 'b' then Some Game.(Player_move SW)
	else if ch == int_of_char 'n' then Some Game.(Player_move SE)
	else None

let run map_seed things_seed =
	let game = Thoodiam.init map_seed things_seed in
	Disp.with_display begin fun disp ->
		let ui_styles, extra_styles = make_styles disp in
		let ui = make_ui extra_styles disp in
		while game.Game.player_alive do
			Ui.draw ui ui_styles disp game;
			match process_input (Disp.get_key disp) with
			| Some c -> Game.update game c
			| None -> ()
		done
	end

let _ =
	let map_seed = ref 0 in
	let things_seed = ref 0 in
	Args.(parse [
			"-map-seed", Set_int map_seed,
				"map generation seed";
			"-things-seed", Set_int map_seed,
				"thing generation seed";
		] []);
	run !map_seed !things_seed
