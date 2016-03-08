module Make =
	functor (D : Display.I) ->
	struct
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

		let draw ui styles disp game =
			draw_panel styles ui.panel;
			draw_status styles ui.status;
			begin match game.Game.player with
			| Some p -> draw_map styles ui.map game p.Game.Being.at
			| None -> ()
			end;
			D.Text_view.refresh ui.panel;
			D.Text_view.refresh ui.status;
			D.Chars_view.refresh ui.map

		module Key =
			struct
				type keys =
					| N | S | E | W | NE | NW | SE | SW
					| Pick_up
					| Quit
			end

		let handle_input game =
			Key.(function
			| N -> [Game.(Move N)]
			| S -> [Game.(Move S)]
			| E -> [Game.(Move E)]
			| W -> [Game.(Move W)]
			| NE -> [Game.(Move NE)]
			| NW -> [Game.(Move NW)]
			| SE -> [Game.(Move SE)]
			| SW -> [Game.(Move SW)]
			| Pick_up ->
				begin match game.Game.player with
				| None -> []
				| Some player ->
					let at = player.Game.Being.at in
					let things = Game.((Map.get game.map at).Cell.things) in
					begin match things with
					| [] -> []
					| t::_ -> [Game.(Pick_up t)]
					end
				end
			| Quit -> [Game.Quit]
			)
	end
