open Std
open Game_data
open Game_state
open Game_changes

module Make =
	functor (D : Display.I) ->
	struct
		let panel_width = 16
		let status_height = 6

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
						map_target : style;
						map_targetable : style;
					}
			end

		module Key =
			struct
				type t =
					| N | S | E | W | NE | NW | SE | SW
					| Pick_up
					| Smart_pick_up
					| Inventory
					| Equipment
					| Drop
					| Throw
					| Smart_throw
					| Next_target
					| Accept_target
					| Cancel_target
					| Page_up
					| Page_down
					| Finish
					| Yes
					| No
					| List_item of int
					| Sel_all
					| Down_stairs
					| Up_stairs
					| Wait
					| Quit
					| Help

				let to_string =
					function
					| N -> "north"
					| S -> "south"
					| E -> "east"
					| W -> "west"
					| NE -> "north east"
					| NW -> "north west"
					| SE -> "south east"
					| SW -> "south west"
					| Pick_up -> "pick up"
					| Smart_pick_up -> "pick up throwables"
					| Inventory -> "inventory"
					| Equipment -> "equipment"
					| Drop -> "drop"
					| Throw -> "throw"
					| Smart_throw -> "throw throwables"
					| Next_target -> "next target"
					| Accept_target -> "accept target"
					| Cancel_target -> "cancel targeting"
					| Page_up -> "page up"
					| Page_down -> "page down"
					| Finish -> "finish"
					| Yes -> "yes"
					| No -> "no"
					| List_item i -> Printf.sprintf "list item %i" i
					| Sel_all -> "select all"
					| Down_stairs -> "down stairs"
					| Up_stairs -> "up stairs"
					| Wait -> "wait"
					| Quit -> "quit"
					| Help -> "help"
			end

		type target_reason = Want_throw of Thing.t

		let letter_list_ids =
			let s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
			Array.init (String.length s) (fun i -> String.make 1 s.[i])

		let uniq = List.fold_left (fun ys y -> if List.mem y ys then ys else y::ys) []

		let key_to_string input_to_string =
			function
			| None -> "{no key}"
			| Some i -> input_to_string i

		let make_intro_help_text input_to_string cur_key_bindings all_key_bindings =
			let key_to_string = key_to_string input_to_string in
			let help_keys = List.filter_map begin fun b ->
					match Key_bindings.get_inv b Key.Help with
					| Some _ as oi -> Some (key_to_string oi)
					| None -> None
				end all_key_bindings in
			[
				Printf.sprintf "press %s to continue"
					(key_to_string (Key_bindings.get_inv cur_key_bindings Key.Finish));
				Printf.sprintf "press %s to get help"
					(English.strings_list_bare "or" "{no key}" (uniq help_keys));
			]

		let make_death_help_text input_to_string cur_key_bindings =
			let key_to_string = key_to_string input_to_string in
			[
				Printf.sprintf "press %s to continue"
					(key_to_string (Key_bindings.get_inv cur_key_bindings Key.Finish));
			]

		type message =
			| Mystery_nonexistance
			| Dead
			| Left
			| Won
			| Nothing_in_range
			| See_here of Thing.t list

		type 'a t =
			{
				panel : D.Text_view.t;
				status : D.Text_view.t;
				map : D.Chars_view.t;
				mutable target_reason : target_reason option;
				mutable target : Map.Location.t option;
				mutable last_target : Map.Location.t option;
				mutable targetable_points : Map.Location.t list; 
				do_popup : ?is_list:bool -> ?show_help:bool -> 'a t -> (D.Text_view.t -> Key.t option -> bool) -> unit;
				styles : Styles.t;
				list_ids : string array;
				mutable messages : message list;
				input_to_string : 'a -> string;
			}

		let join_opt_strings opt_strs =
			String.concat " " (List.filter_map (fun x -> x) opt_strs)

		let string_of_effective_weapon_stats accuracy (_, damage) evasion =
			let p = Printf.sprintf in
			p "(%s,%s)%s"
				Combat.(int_factors_to_string accuracy)
				Weapon.(Combat.(Mod_dice.to_string damage))
				(if evasion != 0 then p " [%i]" evasion else "")

		let string_of_armour_main_stats thing =
			let open Thing in
			let open Thing_kind in
			let evasion =
				match Thing.in_combat thing with
				| Some ic -> ic.In_combat.evasion
				| None -> 0 in
			String.concat " "
				begin
					List.filter_map begin
						Opt.map begin fun melee ->
							Printf.sprintf "[%i,%s]" evasion Armour.(Dice.to_string melee.protection)
						end
					end [thing.kind.body_armour; thing.kind.shield; thing.kind.helm]
				end

		let string_of_combat_stats thing =
			let open Game_data in
			let open In_combat in
			let open Thing in
			let open Thing_kind in
			let p = Printf.sprintf in
			let accuracy, evasion =
				match in_combat thing with
				| Some ic -> ic.accuracy, ic.evasion
				| None -> 0, 0 in
			let melee_strs =
				List.filter_map begin
					Opt.map begin fun melee ->
						p "(%i,%s)" accuracy Weapon.(Dice.to_string melee.damage)
					end
				end [thing.kind.melee] in
			let armour_strs =
				List.filter_map begin
					Opt.map begin fun melee ->
						p "[%i,%s]" evasion Armour.(Dice.to_string melee.protection)
					end
				end [thing.kind.body_armour; thing.kind.shield; thing.kind.helm] in
			match melee_strs, armour_strs with
			| _::_, _::_ ->
				p "%s %s"
					(String.concat " " melee_strs)
					(String.concat " " armour_strs)
			| _::_, [] ->
				p "%s%s"
					(String.concat " " melee_strs)
					(if evasion != 0 then p " [%i]" evasion else "")
			| [], _::_ ->
				p "%s%s"
					(if accuracy != 0 then p "(%i) " accuracy else "")
					(String.concat " " armour_strs)
			| [], [] ->
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

		let string_of_thing_seen = string_of_thing_inv

		let string_of_thing_throwable ?min_dist being thing =
			Thing.(
				let range = Being.throw_range being thing in
				Printf.sprintf "%s%s [range:%0.2f]%s"
					(string_of_thing_inv thing)
					(if is_throwing thing then " [throwable]" else "")
					range
					(match min_dist with Some md when md <= range -> " [in range]" | _ -> "")
			)

		let string_of_slot being equip_slot =
			Printf.sprintf "%s: %s%s%s"
				equip_slot.Equip_slot.name
				begin match Being.(in_slot being equip_slot) with
					| Some t -> string_of_thing_inv t
					| None -> ""
				end
				begin
					if Being.can_use being equip_slot then ""
					else " [can't use]"
				end
				begin
					if Equip_slot.(equip_slot.kind = Hand) then begin
						Being.(match weapon_state being equip_slot with
						| No_weapon -> ""
						| One_handed -> " [one handed]"
						| Two_handed -> " [two handed]"
						| Need_hand -> " [need two hands]"
						)
					end else ""
				end

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

		let show_info ?(can_finish=true) ?(on_finish=fun _ -> ()) ?(extra_text=[]) title text ui =
			ui.do_popup ui begin fun view key ->
				let continue =
					Key.(match key with
					| None -> true
					| Some Finish | Some Page_down ->
						if can_finish then begin
							on_finish ();
							false
						end else
							true
					| _ -> true
					) in
				D.Text_view.clear view;
				D.Text_view.draw view ~style:ui.styles.Styles.popup_label (1, 1) title;
				let dimx, dimy = D.Text_view.dim view in
				let lines = wrap_string (dimx / 10) (dimx - 2) text in
				let n = List.length lines in
				List.iteri begin fun i line ->
					D.Text_view.draw view ~style:ui.styles.Styles.popup_text (1, 3 + i) line
				end lines;
				List.iteri begin fun i line ->
					D.Text_view.draw view ~style:ui.styles.Styles.popup_key (1, 4 + n + i) line
				end extra_text;
				D.Text_view.refresh view;
				continue
			end

		let show_confirm title text ui default f =
			ui.do_popup ui begin fun view key ->
				let continue =
					Key.(match key with
					| None -> true
					| Some key ->
						begin match key with
						| Finish ->
							f default;
							false
						| Yes ->
							f true;
							false
						| No ->
							f false;
							false
						| _ -> true
						end
					) in
				D.Text_view.clear view;
				D.Text_view.draw view ~style:ui.styles.Styles.popup_label (1, 1) title;
				let dimx, dimy = D.Text_view.dim view in
				let lines = wrap_string (dimx / 10) (dimx - 2) text in
				List.iteri begin fun i line ->
					D.Text_view.draw view ~style:ui.styles.Styles.popup_text (1, 3 + i) line
				end lines;
				D.Text_view.draw view ~style:ui.styles.Styles.popup_key (1, 4 + List.length lines) "yes/no?";
				D.Text_view.refresh view;
				continue
			end

		let show_list ?(show_help) ?(start_sel_all=false) title to_string list ?(multiple=false) ?(select=true) ?(repeat=false) ui f =
			let start_i = ref 0 in
			let len = List.length list in
			let all_indices = List.init len (fun i -> i) in
			let sel = ref (if start_sel_all then all_indices else []) in
			let max_id_len = Array.fold_left (fun m k -> max m (String.length k)) 0 ui.list_ids in
			ui.do_popup ~is_list:true ?show_help ui begin fun view key ->
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
					| Finish ->
						finish ()
					| Page_up ->
						start_i := max 0 (!start_i - page_size)
					| Page_down ->
						start_i := let i = !start_i + page_size in if i >= len then !start_i else i
					| Sel_all ->
						if List.length !sel >= len then
							sel := []
						else
							sel := all_indices
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

		let show_key_bindings key_bindings ui =
			let strs =
				Key_bindings.to_strings_list ui.input_to_string Key.to_string key_bindings in
			show_list ~show_help:false "Help" (fun (i, o) -> Printf.sprintf "%s -> %s" i o) ~select:false strs ui begin fun _ ->
				()
			end

		let string_of_game_message =
			let p = Printf.sprintf in
			let thing = string_of_thing in
			let being b = Thing.(name Being.(body b)) in
			Game_changes.(Message.(function
			| Pick_up (b, t) -> p "The %s picks up the %s." (being b) (thing t)
			| Hit (a, d, hp, r) -> p "The %s hits the %s for %i damage: %s." (being a) (being d) hp (Combat.Result.to_desc r)
			| Miss (a, d, r) -> p "The %s misses the %s: %s." (being a) (being d) (Combat.Result.to_desc r)
			| Drop (b, t) -> p "The %s drops up the %s." (being b) (thing t)
			| Equip (b, _, t) -> p "The %s equips up the %s." (being b) (thing t)
			| Unequip (b, _, t) -> p "The %s unequips up the %s." (being b) (thing t)
			| Take_stairs (b, d) -> p "The %s takes the stairs %s." (being b) (match d with Up -> "up" | Down -> "down")
			| Die b -> p "The %s dies." (being b)
			))

		let string_of_ui_message =
			let p = Printf.sprintf in
			function
			| Mystery_nonexistance -> "You have ceased to exist for an unknown reason!"
			| Dead -> "You are dead!"
			| Left -> "You lost by leaving the dungeon!"
			| Won -> "You have won!"
			| Nothing_in_range -> "Nothing is in range."
			| See_here t -> p "There is %s here." (English.strings_list (List.map string_of_thing_seen t))

		let draw_panel styles game view =
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
					let rf x = string_of_int (int_of_float x) in
					Opt.iter begin fun being ->
						draw_triple "Inv" (rf being.inv_weight) (rf being.can_carry)
					end game.player
				);
				draw_space ();
				Opt.iter begin fun player ->
					let equips = Being.get_usable_equips player in
					List.iter begin fun (slot, thing) ->
						if Thing.is_weapon thing then
							let accuracy, damage = Combat.effective_weapon player slot in
							let evasion = match Thing.in_combat thing with Some ic -> ic.In_combat.evasion | None -> 0 in
							draw_line (string_of_effective_weapon_stats accuracy damage evasion)
					end equips;
					List.iter begin fun (_, thing) ->
						if Thing.is_armour thing then
							draw_line (string_of_armour_main_stats thing)
					end equips
				end game.player
			)

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

		let draw_map ui view game centre =
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
						if match ui.target with Some t -> wp = t | None -> false then
							ui.styles.Styles.map_target
						else if List.mem wp ui.targetable_points then
							ui.styles.Styles.map_targetable
						else if Game.(Map.is_valid game.player_info.Player_info.seen wp)
							&& Game.(Map.get game.player_info.Player_info.fov wp)
							then ui.styles.Styles.map_fov
						else ui.styles.Styles.map_seen in
					V.draw view ~style:style sp char
				done
			done

		let make ~panel ~status ~map ~do_popup ~styles ~list_ids ~input_to_string =
			{
				panel = panel;
				status = status;
				map = map;
				target_reason = None;
				target = None;
				last_target = None;
				targetable_points = [];
				do_popup = do_popup;
				styles = styles;
				list_ids = list_ids;
				messages = [];
				input_to_string = input_to_string;
			}

		let draw ui disp game =
			draw_panel ui.styles game ui.panel;
			draw_status ui.styles game.Game.region.Region.messages ui.messages ui.status;
			begin match game.Game.player with
			| Some p -> draw_map ui ui.map game p.Being.at
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
				| Won -> ui.messages <- Won::ui.messages
				)
			| Some player ->
				let at = player.Being.at in
				let things_here =
					List.filter (fun t -> t != player.Being.body)
						(Game.(Region.((Map.get game.region.map at).Cell.things))) in
				if not (List.is_empty things_here) then
					ui.messages <- (See_here things_here) :: ui.messages

		let points_dist p1 p2 = 
			Vec.(dist (float_of_int p1) (float_of_int p2))

		let closest_point p points =
			fst @@ List.fold_left begin fun (_, best_dist as best) point ->
				let dist = points_dist point p in
				if dist < best_dist then (Some point, dist)
				else best
			end (None, infinity) points

		let targets_in_range game player max_range =
			List.filter_map begin fun b ->
				let p = b.Being.at in
				if points_dist player.Being.at p < max_range then Some p
				else None
			end Game.(game.player_info.Player_info.fov_beings)

		let min_target_dist game player =
			List.fold_left (fun d p -> min d (points_dist p player.Being.at)) infinity (targets_in_range game player infinity)

		let handle_player_input game player key_bindings ui key do_cmd =
			let start_target max_range reason =
				let targetable_points =
					begin
						List.sort begin fun (x1, y1) (x2, y2) ->
							let a = x1 - x2 in
							if a != 0 then a
							else
								y1 - y2
						end (targets_in_range game player max_range)
					end in
				match targetable_points with
				| [] ->
					ui.messages <- Nothing_in_range::ui.messages
				| _ ->
					ui.targetable_points <- targetable_points;
					ui.target_reason <- Some reason;
					let start_point =
						match ui.last_target with
						| Some p when Map.get Game.(game.player_info.Player_info.fov) p -> p
						| _ -> player.Being.at in
					ui.target <- closest_point start_point ui.targetable_points in
			let movement dir =
				match ui.target with
				| None ->
					let p1 = Vec.(player.Being.at + Direction.to_vec dir) in
					if List.exists (fun b -> b.Being.at = p1) Game.(game.region.Region.beings) then do_cmd Action.(Melee_attack dir)
					else do_cmd Action.(Move dir)
				| Some p ->
					let p1 = Vec.(p + Direction.to_vec dir) in
					match closest_point p1 (List.filter (fun p3 -> p3 != p) ui.targetable_points) with
					| None -> ()
					| Some new_target ->
						ui.target <- Some new_target in
			let do_pick_up title ?start_sel_all pred =
				let at = player.Being.at in
				let things = Game.(Region.(((Map.get game.region.map at).Cell.things))) in
				begin match List.filter (fun t -> t != player.Being.body && pred t) things with
				| [] -> ()
				| [t] -> do_cmd Action.(Pick_up t)
				| ts ->
					show_list title string_of_thing_inv ts ?start_sel_all ~multiple:true ui begin fun thing ->
						do_cmd Action.(Pick_up thing)
					end
				end in
			Key.(match key with
			| N -> movement Direction.N
			| S -> movement Direction.S
			| E -> movement Direction.E
			| W -> movement Direction.W
			| NE -> movement Direction.NE
			| NW -> movement Direction.NW
			| SE -> movement Direction.SE
			| SW -> movement Direction.SW
			| Wait ->
				do_cmd Action.Wait
			| Help ->
				show_key_bindings key_bindings ui
			| Quit ->
				show_confirm "Quit" "Quit and kill this character?" ui false begin function
					| true ->
						do_cmd Action.Quit
					| false -> ()
				end
			| Inventory ->
				show_list "Inventory" string_of_thing_inv Being.(inv player) ~select:false ui begin fun _ ->
					()
				end
			| Equipment ->
				show_list "Equipment" (string_of_slot player) Being.(equip_slots player) ~repeat:true ui begin fun equip_slot ->
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
				do_pick_up "Pick up" (fun _ -> true)
			| Smart_pick_up ->
				do_pick_up "Pick up throwables" ~start_sel_all:true (fun t -> Thing.is_throwing t)
			| Up_stairs -> begin
					if List.mem player.Being.at game.Game.region.Region.up_stairs then begin
						if game.Game.on_level > 0 then begin
							do_cmd Action.(Take_stairs Up)
						end else begin
							show_confirm "Leave dungeon" "Leave the dungeon? You will win if you have the Thoodiam, and lose otherwise." ui false begin function
								| true ->
									do_cmd Action.(Take_stairs Up)
								| false -> ()
							end
						end
					end
				end
			| Down_stairs -> begin
					if List.mem player.Being.at game.Game.region.Region.down_stairs then
						do_cmd Action.(Take_stairs Down)
				end
			| Throw ->
				let min_dist = min_target_dist game player in
				show_list "Thing to throw" (string_of_thing_throwable ~min_dist player) Being.(inv player) ~multiple:true ui begin fun thing ->
					start_target (Being.throw_range player thing) (Want_throw thing)
				end
			| Smart_throw ->
				let min_dist = min_target_dist game player in
				let useful_inv =
					List.filter begin fun thing ->
						Thing.is_throwing thing
						&& Being.throw_range player thing >= min_dist
					end Being.(inv player) in
				show_list "Thing to throw (throwables in range)" (string_of_thing_throwable ~min_dist player) useful_inv ~multiple:true ui begin fun thing ->
					start_target (Being.throw_range player thing) (Want_throw thing)
				end
			| Next_target ->
				Opt.iter begin fun target ->
					let i =
						match list_index target ui.targetable_points with
						| Some i -> i
						| None -> 0 in
					let j = (i + 1) mod List.length ui.targetable_points in
					try
						ui.target <- Some (List.nth ui.targetable_points j)
					with Failure _ -> ()
				end ui.target
			| Accept_target ->
				Opt.iter begin fun target ->
					Opt.iter begin fun being ->
						begin match ui.target_reason with
						| Some (Want_throw thing) ->
							begin match List.find_pred (fun b -> b.Being.at = target) game.Game.region.Region.beings with
							| None -> ()
							| Some target_being -> do_cmd Action.(Thrown_attack (thing, target, target_being))
							end
						| None -> ()
						end;
						ui.last_target <- ui.target;
						ui.target_reason <- None;
						ui.target <- None;
					ui.targetable_points <- []
					end (List.find_pred (fun b -> b.Being.at = target) Game.(game.player_info.Player_info.fov_beings))
				end ui.target
			| Cancel_target ->
				Opt.iter begin fun target ->
					ui.last_target <- ui.target;
					ui.target_reason <- None;
					ui.target <- None;
					ui.targetable_points <- []
				end ui.target
			| _ -> ()
			)

		let handle_input game key_bindings ui key do_cmd =
			match game.Game.player with
			| None ->
				begin match key with
				| Key.Quit -> Game.(game.status <- Lost Died)
				| _ -> ()
				end
			| Some player ->
				handle_player_input game player key_bindings ui key do_cmd
	end
