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
			panel_text = make disp ~fg:(c "black");
			status_text = make disp ~fg:(c "black");
			popup_label = make disp ~fg:(c "yellow");
			popup_key = make disp ~fg:(c "yellow");
			popup_text = make disp ~fg:(c "black");
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

let do_popup disp extra_styles parent_win ui f =
	let win = Disp.Window.make disp parent_win (0, 0) (Disp.Window.dim parent_win) in
	let view = Disp.Text_view.make disp win in
	Disp.Text_view.config ~bg:extra_styles.Styles.popup_bg view;
	f view;
	ignore (Disp.get_key disp)

let make_ui ui_styles extra_styles disp =
	let root = Disp.root disp in
	let panel_win, rest_win = Disp.Window.split disp root Disp.Horiz 0.1 in
	let map_win, status_win = Disp.Window.split disp rest_win Disp.Vert 0.9 in
	let ui = Ui.make
			~panel:(Disp.Text_view.make disp panel_win)
			~status:(Disp.Text_view.make disp status_win)
			~map:(Disp.Chars_view.make disp map_win)
			~styles:ui_styles
			~do_popup:(do_popup disp extra_styles map_win) in
	Disp.Text_view.config ~bg:extra_styles.Styles.panel_bg ui.Ui.panel;
	Disp.Text_view.config ~bg:extra_styles.Styles.status_bg ui.Ui.status;
	Disp.Chars_view.config ~bg:extra_styles.Styles.map_bg ui.Ui.map;
	ui

let show_death disp styles =
	let root = Disp.root disp in
	let win = Disp.Window.make disp root (0, 0) (Disp.Window.dim root) in
	let view = Disp.Text_view.make disp win in
	Disp.Text_view.config ~bg:styles.Styles.death_bg view;
	Disp.Text_view.clear view;
	Disp.Text_view.draw view ~style:styles.Styles.death_text (1, 1) "You are dead.";
	Disp.Text_view.refresh view

let process_input key =
	Ui.Key.(
		if key = 81 then Some Quit
		else if key = 71 then Some Pick_up
		else if key = 73 then Some Inventory
		else if key = 38 || key = 75 then Some N
		else if key = 40 || key = 74 then Some S
		else if key = 37 || key = 72 then Some W
		else if key = 39 || key = 76 then Some E
		else if key = 89 then Some NW
		else if key = 85 then Some NE
		else if key = 66 then Some SW
		else if key = 78 then Some SE
		else begin
			Printf.eprintf "unknown key %i\n" key;
			None
		end
	)

let onload _ =
	let map_seed = 0 in
	let things_seed = 0 in
	let game = Thoodiam.init map_seed things_seed in
	let disp = Disp.init Dom_html.document##body in
	let ui_styles, extra_styles = make_styles disp in
	let ui = make_ui ui_styles extra_styles disp in
	Ui.draw ui disp game;
	Disp.input_loop disp begin fun event ->
		begin match event with
		| None -> ()
		| Some key ->
			begin match process_input key with
			| Some k ->
				Game.update game (Ui.handle_input game ui k);
				Ui.update_game game ui
			| None -> ()
			end
		end;
		Ui.draw ui disp game;
		begin match game.Game.player with
		| Some _ -> true
		| None ->
			show_death disp extra_styles;
			false
		end
	end;
	Js._false

let _ =
	Dom_html.window##onload <- Dom_html.handler onload
