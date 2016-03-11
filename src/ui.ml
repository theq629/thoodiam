open Std
open Game_data
open Game_state
open Game_changes

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
					| Down_stairs
					| Up_stairs
					| Wait
					| Quit
			end

		let letter_list_ids =
			let s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
			Array.init (String.length s) (fun i -> String.make 1 s.[i])

		type message =
			| Mystery_nonexistance
			| Dead
			| Left
			| See_here of Thing.t list

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

		let string_of_combat_stats thing =
			let open Game_data in
			let open In_combat in
			let open Weapon in
			let open Armour in
			let p = Printf.sprintf in
			let accuracy, evasion =
				match Thing.(in_combat thing) with
				| Some ic -> ic.accuracy, ic.evasion
				| None -> 0, 0 in
			match (Thing.melee thing), (Thing.armour thing) with
			| Some w, Some a ->
				p "(%i,%s) [%i,%s]" accuracy (Dice.to_string w.damage) evasion (Dice.to_string a.protection)
			| Some w, None ->
				p "(%i,%s)%s" accuracy (Dice.to_string w.damage) (if evasion != 0 then p " [%i]" evasion else "")
			| None, Some a ->
				p "%s[%i,%s]" (if accuracy != 0 then p "(%i) " accuracy else "") evasion (Dice.to_string a.protection)
			| None, None ->
				p ""

		let string_of_thing thing =
			let cs = string_of_combat_stats thing in
			match cs with
			| "" -> Thing.(name thing)
			| _ -> Printf.sprintf "%s %s" Thing.(name thing) cs

		let string_of_thing_inv thing =
			Thing.(
				Printf.sprintf "%s %0.2f" (string_of_thing thing) Thing.(weight thing)
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
			let being b = Thing.(name Being.(body b)) in
			Game_changes.(Message.(function
			| Pick_up (b, t) -> p "The %s picks up the %s." (being b) (thing t)
			| Melee_hit (a, d, hp, r) -> p "The %s hits the %s for %i damage: %s." (being a) (being d) hp (Combat.Result.to_string r)
			| Melee_miss (a, d, r) -> p "The %s misses the %s: %s." (being a) (being d) (Combat.Result.to_string r)
			| Drop (b, t) -> p "The %s drops up the %s." (being b) (thing t)
			| Equip (b, _, t) -> p "The %s equips up the %s." (being b) (thing t)
			| Unequip (b, _, t) -> p "The %s unequips up the %s." (being b) (thing t)
			| Take_stairs (b, d) -> p "The %s takes the stairs %s." (being b) (match d with Up -> "up" | Down -> "down")
			| Die b -> p "The %s dies." (being b)
			))

		let english_format_list list =
			let vowels = "aeiouAEIOU" in
			let add_article str =
				match str with
				| "" -> ""
				| s ->
					try
						ignore (String.index vowels s.[0]);
						"an " ^ s
					with Not_found ->
						"a " ^ s in
			match list with
			| [] -> "nothing"
			| [s] -> add_article s
			| [s1; s2] ->
				String.concat " and " [add_article s1; add_article s2]
			| _ ->
				let num = List.length list in
				String.concat ", " (List.mapi (fun i s -> (if i < num - 1 then "" else "and ") ^ add_article s) list)

		let string_of_ui_message =
			let p = Printf.sprintf in
			function
			| Mystery_nonexistance -> "You have ceased to exist for an unknown reason!"
			| Dead -> "You are dead!"
			| Left -> "You lost by leaving the dungeon!"
			| See_here t -> p "There is %s here." (english_format_list (List.map string_of_thing t))

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
				D.Text_view.draw view ~style:styles.Styles.panel_text (8 + 3 - String.length value, !y) value;
				incr y in
			let draw_triple label value max_value =
				D.Text_view.draw view ~style:styles.Styles.panel_label (1, !y) label;
				D.Text_view.draw view ~style:styles.Styles.panel_text (8 + 3 - String.length value, !y) value;
				D.Text_view.draw view ~style:styles.Styles.panel_text (8 + 3, !y) "/";
				D.Text_view.draw view ~style:styles.Styles.panel_text (8 + 3 + 3 + 1 - String.length max_value, !y) max_value;
				incr y in
			Game.(
				draw_pair "Level" (string_of_int (game.Game.on_level + 1));
				draw_space ();
				Being.(
					Opt.iter begin fun being ->
						draw_triple "HP" (string_of_int being.hp) (string_of_int being.max_hp);
						draw_pair "Stress" (string_of_int being.stress)
					end game.player
				);
				draw_space ();
				Bodyable.(
					Opt.iter begin fun player ->
						Opt.iter begin fun body ->
							draw_pair "Str" (string_of_int body.str);
							draw_pair "Dex" (string_of_int body.dex);
							draw_pair "Con" (string_of_int body.con)
						end Thing.(bodyable Being.(body player))
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
									draw_line (string_of_combat_stats thing)
							end Being.(in_slot player equip_slot)
						end Being.(equip_slots player);
						List.iter begin fun equip_slot ->
							Opt.iter begin fun thing ->
								if equip_slot.Equip_slot.is_armour then
									draw_line (string_of_combat_stats thing)
							end Being.(in_slot player equip_slot)
						end Being.(equip_slots player)
					end game.player
				)
			)

		let wrap_string max_space_adjust len str =
			let n = String.length str in
			let rec prev_space i j =
				if j <= i then None
				else if str.[j] == ' ' then Some j
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
			let clip_list len list =
				let rec run len_left =
					function
					| [] -> []
					| xs when len_left <= len -> xs
					| x::xs -> run (len_left - 1) xs in
				run (List.length list) list in
			let module V = D.Text_view in
			V.clear view;
			(* TODO: we should probably get the messages reversed already *)
			(* TODO: handle scrolling if we totally run out of space *)
			let dimx, dimy = D.Text_view.dim view in
			let msg_strs =
					(List.map string_of_game_message (List.rev game_messages))
					@ (List.map string_of_ui_message ui_messages) in
			let lines = List.flat_map begin fun msg ->
					wrap_string (dimx / 10) (dimx - 3) msg
				end msg_strs in
			let use_lines, have_more =
				if List.length lines > dimy then clip_list dimy lines, true
				else lines, false in
			List.iteri begin fun i line ->
				V.draw view ~style:styles.Styles.status_text (1, i) line
			end use_lines;
			if have_more then begin
				for y = dimy - 1 downto dimy - 3 do
					V.draw view ~style:styles.Styles.status_text (dimx - 1, y) "."
				done
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
				Game.(match game.status with
				| Playing -> ui.messages <- Mystery_nonexistance::ui.messages
				| Lost Died -> ui.messages <- Dead::ui.messages
				| Lost Left -> ui.messages <- Left::ui.messages
				)
			| Some player ->
				let at = player.Being.at in
				let things_here =
					List.filter (fun t -> t != player.Being.body)
						(Game.(Region.((Map.get game.region.map at).Cell.things))) in
				if not (List.is_empty things_here) then
					ui.messages <- (See_here things_here) :: ui.messages

		let handle_player_input game player ui key do_cmd =
			let move_or_attack dir =
				let p1 = Vec.(player.Being.at + Direction.to_vec dir) in
				if List.exists (fun b -> b.Being.at = p1) Game.(game.region.Region.beings) then do_cmd Action.(Melee_attack dir)
				else do_cmd Action.(Move dir) in
			Key.(match key with
			| N -> move_or_attack Direction.N
			| S -> move_or_attack Direction.S
			| E -> move_or_attack Direction.E
			| W -> move_or_attack Direction.W
			| NE -> move_or_attack Direction.NE
			| NW -> move_or_attack Direction.NW
			| SE -> move_or_attack Direction.SE
			| SW -> move_or_attack Direction.SW
			| Wait ->
				do_cmd Action.Wait
			| Quit ->
				ui.user_quit <- true;
				do_cmd Action.Quit
			| Inventory ->
				show_list "Inventory" string_of_thing_inv Being.(inv player) ~select:false ui begin fun _ ->
					()
				end
			| Equipment ->
				let string_of_slot es =
					Printf.sprintf "%s: %s"
						es.Equip_slot.name
						begin match Being.(in_slot player es) with
							| Some t -> string_of_thing_inv t
							| None -> ""
						end in
				show_list "Equipment" string_of_slot Being.(equip_slots player) ~repeat:true ui begin fun equip_slot ->
					match Being.(in_slot player equip_slot) with
					| Some _ ->
						do_cmd Action.(Unequip equip_slot)
					| None ->
						show_list (Printf.sprintf "Equip as %s" equip_slot.Equip_slot.name) string_of_thing_inv Being.(inv player) ui begin fun thing ->
							do_cmd Action.(Equip (thing, equip_slot))
						end
				end
			| Drop ->
				show_list "Drop" string_of_thing_inv Being.(inv player) ~multiple:true ui begin fun thing ->
					do_cmd Action.(Drop thing)
				end
			| Pick_up ->
				let at = player.Being.at in
				let things = Game.(Region.(((Map.get game.region.map at).Cell.things))) in
				begin match List.filter (fun t -> t != player.Being.body) things with
				| [] -> ()
				| [t] -> do_cmd Action.(Pick_up t)
				| ts ->
					show_list "Get" string_of_thing_inv ts ~multiple:true ui begin fun thing ->
						do_cmd Action.(Pick_up thing)
					end
				end
			| Up_stairs -> begin
					if List.mem player.Being.at game.Game.region.Region.up_stairs then
						do_cmd Action.(Take_stairs Up)
				end
			| Down_stairs -> begin
					if List.mem player.Being.at game.Game.region.Region.down_stairs then
						do_cmd Action.(Take_stairs Down)
				end
			| _ -> ()
			)

		let handle_input game ui key do_cmd =
			match game.Game.player with
			| None ->
				begin match key with
				| Key.Quit -> ui.user_quit <- true
				| _ -> ()
				end
			| Some player ->
				handle_player_input game player ui key do_cmd
	end
