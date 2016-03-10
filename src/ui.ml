open Std
open Game_data

module Make =
	functor (D : Display.I) ->
	struct
		module Styles =
			struct
				type style = D.Style.t

				type t =
					{
						panel_label : style;
						panel_text : style;
						status_text : style;
						popup_text : style;
						popup_text_sel : style;
						popup_label : style;
						popup_key : style;
						popup_key_sel : style;
						map_fov : style;
						map_seen : style;
					}
			end

		module Key =
			struct
				type t =
					| N | S | E | W | NE | NW | SE | SW
					| Pick_up
					| Inventory
					| Equipment
					| Drop
					| Page_up
					| Page_down
					| End
					| List_item of int
					| Quit
			end

		let letter_list_ids =
			let s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
			Array.init (String.length s) (fun i -> String.make 1 s.[i])

		type message =
			| Dead
			| See_here of Thing.t

		type t =
			{
				panel : D.Text_view.t;
				status : D.Text_view.t;
				map : D.Chars_view.t;
				do_popup : t -> (D.Text_view.t -> Key.t option -> bool) -> unit;
				styles : Styles.t;
				list_ids : string array;
				mutable messages : message list;
				mutable user_quit : bool;
			}

		let join_opt_strings opt_strs =
			String.concat " " (List.filter_map (fun x -> x) opt_strs)

		let string_of_combat comb =
			let p = Printf.sprintf in
			Combat.(join_opt_strings [
					if not (Dice.is_zero comb.damage) then
						Some (p "(%i,%s)" comb.accuracy (Dice.to_string comb.damage))
					else if comb.accuracy != 0 then
						Some (p "(%i)" comb.accuracy)
					else
						None;
					if not (Dice.is_zero comb.protection) then
						Some (p "[%i,%s]" comb.evasion (Dice.to_string comb.protection))
					else if comb.evasion != 0 then
						Some (p "[%i]" comb.evasion)
					else
						None
				])

		let string_of_thing thing =
			Thing.(Kind.(
				let k = thing.kind in
				join_opt_strings [
						Some k.name;
						Opt.map (fun c -> string_of_combat c) k.melee;
						Opt.map (fun c -> string_of_combat c) k.armour
					]
			))

		let string_of_thing_inv thing =
			Thing.(
				Printf.sprintf "%s %0.2f" (string_of_thing thing) thing.kind.Kind.weight
			)

		let show_list title to_string list ?(multiple=false) ?(select=true) ?(repeat=false) ui f =
			let start_i = ref 0 in
			let sel = ref [] in
			let len = List.length list in
			let max_id_len = Array.fold_left (fun m k -> max m (String.length k)) 0 ui.list_ids in
			ui.do_popup ui begin fun view key ->
				let page_size = min (Array.length ui.list_ids) (let _, dimy = D.Text_view.dim view in dimy - 3) in
				let continue = ref true in
				let go () =
					List.iter (fun i -> f (List.nth list i)) !sel;
					sel := [] in
				let finish () =
					go ();
					continue := false in
				Key.(match key with
				| None -> ()
				| Some key ->
					begin match key with
					| End ->
						finish ()
					| Page_up ->
						start_i := max 0 (!start_i - page_size)
					| Page_down ->
						start_i := let i = !start_i + page_size in if i >= len then !start_i else i
					| List_item i ->
						if select then begin
							let i1 = !start_i + i in
							if i1 >= 0 && i1 < len then begin
								if List.mem i1 !sel then sel := List.filter (fun x -> x != i1) !sel
								else sel := i1::!sel
							end;
							if repeat then
								go ()
							else if not multiple then
								finish ()
						end
					| _ -> ()
					end
				);
				D.Text_view.clear view;
				D.Text_view.draw view ~style:ui.styles.Styles.popup_label (1, 0) title;
				let offset_y =
					if !start_i > 0 then begin
						D.Text_view.draw view ~style:ui.styles.Styles.popup_text (1, 1) "...";
						1
					end else 0 in
				List.iteri begin fun i x ->
					if i >= !start_i && i < min len (!start_i + page_size) then begin
						let y = 1 + offset_y + i - !start_i in
						let key_i = i - !start_i in
						let key_style, text_style =
							if List.mem key_i !sel then ui.styles.Styles.popup_key_sel, ui.styles.Styles.popup_text_sel
							else ui.styles.Styles.popup_key, ui.styles.Styles.popup_text in
						D.Text_view.draw view ~style:key_style (1, y) ui.list_ids.(key_i);
						D.Text_view.draw view ~style:text_style (max_id_len + 2, y) (to_string x)
					end;
				end list;
				if !start_i + page_size < len then
					D.Text_view.draw view ~style:ui.styles.Styles.popup_text (1, 1 + offset_y + page_size) "...";
				D.Text_view.refresh view;
				!continue
			end

		let string_of_game_message =
			let p = Printf.sprintf in
			let thing = string_of_thing in
			let being b = Being.(Thing.((b.body.kind.Kind.name))) in
			Game.(Message.(function
			| Pick_up (b, t) -> p "The %s picks up the %s." (being b) (thing t)
			| Melee_hit (a, d, hp) -> p "The %s hits the %s for %i damage." (being a) (being d) hp
			| Melee_miss (a, d) -> p "The %s misses the %s." (being a) (being d)
			| Drop (b, t) -> p "The %s drops up the %s." (being b) (thing t)
			| Equip (b, _, t) -> p "The %s equips up the %s." (being b) (thing t)
			| Unequip (b, _, t) -> p "The %s unequips up the %s." (being b) (thing t)
			| Die b -> p "The %s dies." (being b)
			))

		let string_of_ui_message =
			let p = Printf.sprintf in
			let thing = string_of_thing in
			function
			| Dead -> "You are dead!"
			| See_here t -> p "There is a %s here." (thing t)

		let draw_panel styles game view =
			let rf x = string_of_int (int_of_float (0.5 +. x)) in
			D.Text_view.clear view;
			let y = ref 0 in
			let draw_space () =
				incr y in
			let draw_line value =
				D.Text_view.draw view ~style:styles.Styles.panel_label (1, !y) value;
				incr y in
			let draw_pair label value =
				D.Text_view.draw view ~style:styles.Styles.panel_label (1, !y) label;
				D.Text_view.draw view ~style:styles.Styles.panel_text (5 + 3 - String.length value, !y) value;
				incr y in
			let draw_triple label value max_value =
				D.Text_view.draw view ~style:styles.Styles.panel_label (1, !y) label;
				D.Text_view.draw view ~style:styles.Styles.panel_text (5 + 3 - String.length value, !y) value;
				D.Text_view.draw view ~style:styles.Styles.panel_text (8, !y) "/";
				D.Text_view.draw view ~style:styles.Styles.panel_text (9 + 3 - String.length max_value, !y) max_value;
				incr y in
			Game.(
				Being.(
					Opt.iter begin fun being ->
						draw_triple "HP" (string_of_int being.hp) (string_of_int being.max_hp);
					end game.player
				);
				draw_space ();
				Bodyable.(
					Opt.iter begin fun player ->
						Opt.iter begin fun body ->
							draw_pair "Str" (string_of_int body.str);
							draw_pair "Dex" (string_of_int body.dex);
							draw_pair "Con" (string_of_int body.con)
						end player.Being.body.Thing.kind.Thing.Kind.bodyable
					end game.player
				);
				draw_space ();
				Being.(
					Opt.iter begin fun being ->
						draw_triple "Inv" (rf being.inv_weight) (rf being.can_carry)
					end game.player
				);
				draw_space ();
				Thing.(
					Opt.iter begin fun player ->
						List.iter begin fun equip_slot ->
							Opt.iter begin fun thing ->
								if equip_slot.Equip_slot.is_melee then
									Opt.iter begin fun c ->
										draw_line (string_of_combat c)
									end thing.kind.Kind.melee
							end (in_slot player equip_slot)
						end player.Being.body.kind.Kind.equip_slots;
						List.iter begin fun equip_slot ->
							Opt.iter begin fun thing ->
								if equip_slot.Equip_slot.is_armour then
									Opt.iter begin fun c ->
										draw_line (string_of_combat c)
									end thing.kind.Kind.armour
							end (in_slot player equip_slot)
						end player.Being.body.kind.Kind.equip_slots
					end game.player
				)
			)

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
			(* TODO: we should probably get the messages reversed already *)
			let msg_str = String.concat " " (
					(List.map string_of_game_message (List.rev game_messages))
					@ (List.map string_of_ui_message ui_messages)
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
			let offset = Vec.(centre - dim / 2) in
			V.clear view;
			for sx = 0 to dimx - 1 do
				for sy = 0 to dimy - 1 do
					let sp = sx, sy in
					let wp = Vec.(sp + offset) in
					let char =
						if Game.(Map.is_valid game.player_info.Player_info.seen wp) then begin
							match Game.(Map.get game.player_info.Player_info.seen wp) with
							| Some t -> t
							| None -> ' '
						end else ' ' in
					let style =
						if Game.(Map.is_valid game.player_info.Player_info.seen wp)
							&& Game.(Map.get game.player_info.Player_info.fov wp)
							then styles.Styles.map_fov
						else styles.Styles.map_seen in
					V.draw view ~style:style sp char
				done
			done

		let make ~panel ~status ~map ~do_popup ~styles ~list_ids =
			{
				panel = panel;
				status = status;
				map = map;
				do_popup = do_popup;
				styles = styles;
				list_ids = list_ids;
				messages = [];
				user_quit = false;
			}

		let draw ui disp game =
			draw_panel ui.styles game ui.panel;
			draw_status ui.styles game.Game.region.Region.messages ui.messages ui.status;
			begin match game.Game.player with
			| Some p -> draw_map ui.styles ui.map game p.Being.at
			| None -> ()
			end;
			D.Text_view.refresh ui.panel;
			D.Text_view.refresh ui.status;
			D.Chars_view.refresh ui.map;
			ui.messages <- []

		let update_game game ui =
			match game.Game.player with
			| None ->
				ui.messages <- Dead::ui.messages
			| Some player ->
				let at = player.Being.at in
				let things = Game.(Region.((Map.get game.region.map at).Cell.things)) in
				List.iter begin fun t ->
					ui.messages <- (See_here t) :: ui.messages
				end (List.filter (fun t -> t != player.Being.body) things)

		let handle_player_input game player ui key do_cmds =
			let move_or_attack dir =
				let p1 = Vec.(player.Being.at + Direction.to_vec dir) in
				if List.exists (fun b -> b.Being.at = p1) Game.(game.region.Region.beings) then do_cmds [Action.(Melee_attack dir)]
				else do_cmds [Action.(Move dir)] in
			Key.(match key with
			| N -> move_or_attack Direction.N
			| S -> move_or_attack Direction.S
			| E -> move_or_attack Direction.E
			| W -> move_or_attack Direction.W
			| NE -> move_or_attack Direction.NE
			| NW -> move_or_attack Direction.NW
			| SE -> move_or_attack Direction.SE
			| SW -> move_or_attack Direction.SW
			| Quit ->
				ui.user_quit <- true;
				do_cmds [Action.Quit]
			| Inventory ->
				show_list "Inventory" string_of_thing_inv player.Being.inv ~select:false ui begin fun _ ->
					()
				end
			| Equipment ->
				let string_of_slot es =
					Printf.sprintf "%s: %s"
						es.Equip_slot.name
						begin match (in_slot player es) with
							| Some t -> string_of_thing_inv t
							| None -> ""
						end in
				show_list "Equipment" string_of_slot (player.Being.body.Thing.kind.Thing.Kind.equip_slots) ~repeat:true ui begin fun equip_slot ->
					match (in_slot player equip_slot) with
					| Some _ ->
						do_cmds [Action.(Unequip equip_slot)]
					| None ->
						show_list (Printf.sprintf "Equip as %s" equip_slot.Equip_slot.name) string_of_thing_inv player.Being.inv ui begin fun thing ->
							do_cmds [Action.(Equip (thing, equip_slot))]
						end
				end
			| Drop ->
				show_list "Drop" string_of_thing_inv player.Being.inv ~multiple:true ui begin fun thing ->
					do_cmds [Action.(Drop thing)]
				end
			| Pick_up ->
				let at = player.Being.at in
				let things = Game.(Region.(((Map.get game.region.map at).Cell.things))) in
				begin match List.filter (fun t -> t != player.Being.body) things with
				| [] -> ()
				| [t] -> do_cmds [Action.(Pick_up t)]
				| ts ->
					show_list "Get" string_of_thing_inv ts ~multiple:true ui begin fun thing ->
						do_cmds [Action.(Pick_up thing)]
					end
				end
			| _ -> do_cmds []
			)

		let handle_input game ui key do_cmds =
			match game.Game.player with
			| None ->
				begin match key with
				| Key.Quit -> ui.user_quit <- true
				| _ -> ()
				end
			| Some player ->
				handle_player_input game player ui key do_cmds
	end
