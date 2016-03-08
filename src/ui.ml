open Std

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

		type message =
			| See_here of Game.Thing.t

		type t =
			{
				panel : D.Text_view.t;
				status : D.Text_view.t;
				map : D.Chars_view.t;
				mutable messages : message list;
			}

		let join_opt_strings opt_strs =
			String.concat " " (List.filter_map (fun x -> x) opt_strs)

		let combat_to_string comb =
			let open Game in
			let p = Printf.sprintf in
			Combat.(join_opt_strings [
					if not (Dice.is_zero comb.damage) then
						Some (p "[%i,%s]" comb.accuracy (Dice.to_string comb.damage))
					else if comb.accuracy != 0 then
						Some (p "[%i]" comb.accuracy)
					else
						None;
					if not (Dice.is_zero comb.protection) then
						Some (p "[%i,%s]" comb.evasion (Dice.to_string comb.protection))
					else if comb.evasion != 0 then
						Some (p "[%i]" comb.evasion)
					else
						None
				])

		let thing_to_string thing =
			let open Game in
			Thing.(Kind.(
				let k = thing.kind in
				join_opt_strings [
						Some k.name;
						Opt.map (fun c -> combat_to_string c) k.melee;
						Opt.map (fun c -> combat_to_string c) k.armour
					]
			))

		let game_message_to_string =
			let p = Printf.sprintf in
			let thing = thing_to_string in
			let being b = Game.Being.(Game.Thing.((b.body.kind.Kind.name))) in
			Game.(Message.(function
			| Pick_up (b, t) -> p "The %s picks up the %s." (being b) (thing t)
			| Drop (b, t) -> p "The %s drops up the %s." (being b) (thing t)
			| Equip (b, _, t) -> p "The %s equips up the %s." (being b) (thing t)
			| Unequip (b, _, t) -> p "The %s unequips up the %s." (being b) (thing t)
			| Die b -> p "the %s dies" (being b)
			))

		let ui_message_to_string =
			let p = Printf.sprintf in
			let thing = thing_to_string in
			function
			| See_here t -> p "There is a %s here." (thing t)

		let draw_panel styles view =
			let module V = D.Text_view in
			V.clear view;
			V.draw view ~style:styles.Styles.panel_text (1, 0) "Info"

		let wrap_string max_space_adjust len str =
			let n = String.length str in
			let rec prev_space i j =
				if j <= i then None
				else if str.[j] == '_' then Some j
				else prev_space i (j - 1) in
			let rec run i lines =
				let j = min (i + len) (String.length str) in
				if j <= i then lines
				else
					let j1, k =
						if j == n then j, j
						else
							match prev_space i j with
							| Some j1 when j - j1 <= max_space_adjust -> j1, j1 + 1
							| _ -> j, j in
					let line = String.sub str i (j1 - i) in
					run k (line::lines) in
			List.rev (run 0 [])

		let draw_status styles game_messages ui_messages view =
			let module V = D.Text_view in
			V.clear view;
			let msg_str = String.concat " " (
					(List.map game_message_to_string game_messages)
					@ (List.map ui_message_to_string ui_messages)
				) in
			(* TODO: handle scrolling if we totally run out of space *)
			if String.length msg_str > 0 then begin
				let dimx, _ = D.Text_view.dim view in
				let lines = wrap_string (dimx / 10) (dimx - 2) msg_str in
				List.iteri begin fun i line ->
					V.draw view ~style:styles.Styles.status_text (1, i) line
				end lines
			end

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

		let make ~panel ~status ~map =
			{
				panel = panel;
				status = status;
				map = map;
				messages = [];
			}

		let draw ui styles disp game =
			draw_panel styles ui.panel;
			draw_status styles game.Game.messages ui.messages ui.status;
			begin match game.Game.player with
			| Some p -> draw_map styles ui.map game p.Game.Being.at
			| None -> ()
			end;
			D.Text_view.refresh ui.panel;
			D.Text_view.refresh ui.status;
			D.Chars_view.refresh ui.map;
			ui.messages <- []

		module Key =
			struct
				type keys =
					| N | S | E | W | NE | NW | SE | SW
					| Pick_up
					| Quit
			end

		let update_game game ui =
			match game.Game.player with
			| None -> ()
			| Some player ->
				let at = player.Game.Being.at in
				let things = Game.((Map.get game.map at).Cell.things) in
				List.iter begin fun t ->
					ui.messages <- (See_here t) :: ui.messages
				end (List.filter (fun t -> t != player.Game.Being.body) things)

		let handle_player_input game player =
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
				let at = player.Game.Being.at in
				let things = Game.((Map.get game.map at).Cell.things) in
				begin match List.filter (fun t -> t != player.Game.Being.body) things with
				| [] -> []
				| t::_ -> [Game.(Pick_up t)]
				end
			| Quit -> [Game.Quit]
			)

		let handle_input game key =
			match game.Game.player with
			| None -> []
			| Some player ->
				handle_player_input game player key
	end
