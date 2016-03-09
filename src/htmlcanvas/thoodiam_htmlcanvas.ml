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
			panel_text = make disp ~fg:(c "black");
			status_text = make disp ~fg:(c "black");
			popup_label = make disp ~fg:(c "yellow");
			popup_key = make disp ~fg:(c "yellow");
			popup_text = make disp ~fg:(c "white");
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
		else if key == 68 then Some Drop
		else if key == 69 then Some Equip
		else if key == 84 then Some Unequip
		else begin
			Printf.eprintf "unknown key %i\n" key;
			None
		end
	)

let process_popup_input key =
	Ui.Key.(
		if key = 27 || key = 13 then Some End
		else if key = 32 || key = 221 then Some Page_down
		else if key = 219 then Some Page_up
		else begin
			Printf.eprintf "unknown key %i\n" key;
			None
		end
	)

let make_ui ui_styles extra_styles disp update_queue =
	let root = Disp.root disp in
	let panel_win, rest_win = Disp.Window.split disp root Disp.Horiz 0.1 in
	let map_win, status_win = Disp.Window.split disp rest_win Disp.Vert 0.9 in
	let do_popup ui f =
		let win = Disp.Window.make disp map_win (0, 0) (Disp.Window.dim map_win) in
		let view = Disp.Text_view.make disp win in
		Disp.Text_view.config ~bg:extra_styles.Styles.popup_bg view;
		let update key =
			if f view (Opt.flat_map process_popup_input key) then ()
			else begin
				update_queue :=
					begin match !update_queue with
					| u::us -> us 
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
		Ui.handle_input game ui key begin fun cmds ->
			Game.update game cmds;
			Ui.update_game game ui
		end
	end (Opt.flat_map process_input key);
	Ui.draw ui disp game

let onload _ =
	let map_seed = 0 in
	let things_seed = 0 in
	let game = Thoodiam.init map_seed things_seed in
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

let _ =
	Dom_html.window##onload <- Dom_html.handler onload
