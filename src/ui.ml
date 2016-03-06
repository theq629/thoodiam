module D = Display_curses

module Styles =
	struct
		type style = D.Style.t

		type t =
			{
				panel_text : style;
				status_text : style;
				map_fov : style;
				map_seen : style;
			}
	end

type t =
	{
		panel : D.Text_view.t;
		status : D.Text_view.t;
		map : D.Chars_view.t;
	}

let draw_panel styles view =
	let module V = D.Text_view in
	V.clear view;
	V.draw view ~style:styles.Styles.panel_text (1, 0) "Info"

let draw_status styles view =
	let module V = D.Text_view in
	V.clear view;
	V.draw view ~style:styles.Styles.status_text (1, 0) "Status"

let draw_map styles view game centre =
	let module V = D.Chars_view in
	let dimx, dimy as dim = V.dim view in
  let offset = Game.Vec.(centre - dim / 2) in
	V.clear view;
  for sx = 0 to dimx - 1 do
    for sy = 0 to dimy - 1 do
      let sp = sx, sy in
      let wp = Game.Vec.(sp + offset) in
			let char =
				if Game.(Game.Map.is_valid game.Game.player_seen wp) then begin
					match Game.Map.get game.Game.player_seen wp with
					| Some t -> t
					| None -> ' '
				end else ' ' in
			let style =
				if Game.(Game.Map.is_valid game.Game.player_seen wp)
					&& Game.Map.get game.Game.player_fov wp
					then styles.Styles.map_fov
				else styles.Styles.map_seen in
			V.draw view ~style:style sp char
		done
	done

let init disp =
	let root = D.root disp in
	let panel_win, rest_win = D.Window.split root D.Left (D.Text_view.screen_dim (10, 10)) in
	let status_win, map_win = D.Window.split rest_win D.Bottom (D.Text_view.screen_dim (1, 1)) in
	{
		panel = D.Text_view.make disp panel_win;
		map = D.Chars_view.make disp map_win;
		status = D.Text_view.make disp status_win;
	}

let draw ui styles disp game =
	draw_panel styles ui.panel;
	draw_status styles ui.status;
	draw_map styles ui.map game game.Game.player_at;
	D.Text_view.refresh ui.panel;
	D.Text_view.refresh ui.status;
	D.Chars_view.refresh ui.map
