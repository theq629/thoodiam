open Std
open Game_data
open Game_state
open Game_changes

type lose_reason = Died | Left
type game_status = Playing | Lost of lose_reason | Won

module Player_info =
	struct
		type t =
			{
				seen : tile option Map.t;
				fov : bool Map.t;
				mutable fov_beings : Being.t list;
			}

		let make region =
			Region.({
				seen = Map.map region.map (fun _ v -> None);
				fov = Map.map region.map (fun _ _ -> false);
				fov_beings = [];
			})
	end

let update_vision region player_info player =
	let open Region in
	let open Player_info in
	let set_visible p =
		if Map.is_valid region.map p then begin
			Map.set player_info.fov p true;
			let cell = Map.get region.map p in
			let tile =
				let rec run =
					function
					| [] -> cell.Cell.terrain.Terrain.tile
					| [t] -> Thing.(tile t)
					| t::_ when Thing.(blocks t) -> Thing.(tile t)
					| _::ts1 -> run ts1 in
				run cell.Cell.things in
			Map.set player_info.seen p (Some tile)
		end in
	let blocks_sight p =
		not (Map.is_valid region.map p)
		|| (Map.get region.map p).Cell.terrain.Terrain.blocking in
	Map.update player_info.fov (fun _ _ -> false);
	Opt.iter begin fun bodyable ->
		set_visible player.Being.at;
		Fov.compute blocks_sight set_visible player.Being.at bodyable.Bodyable.vision
	end Thing.(bodyable Being.(body player));
	player_info.fov_beings <-
		List.filter begin fun being ->
			being != player && Map.get player_info.fov being.Being.at
		end region.Region.beings

type t =
	{
		rng : Rng.Source.t;
		make_level : int -> Region.t;
		check_win : t -> bool;
		mutable status : game_status;
		mutable on_level : int;
		mutable levels : (int * Region.t) list;
		mutable saved_player_info : (int * Player_info.t) list;
		mutable ai : Ai.t;
		mutable region : Region.t;
		mutable player : Being.t option;
		mutable player_info : Player_info.t;
	}

let set_level game level_i =
	if level_i < 0 then begin
		game.status <-
			if game.check_win game then Won
			else Lost Left;
		game.player <- None;
	end else begin
		let region =
			try List.assoc level_i game.levels
			with Not_found ->
				let new_region = game.make_level level_i in
				game.levels <- (level_i, new_region)::game.levels;
				new_region in
		Region.revisit region;
		Opt.iter begin fun player ->
			let from_stairs, to_stairs =
				Region.(
					if level_i < game.on_level then game.region.up_stairs, region.down_stairs
					else game.region.down_stairs, region.up_stairs
				) in
			assert (match from_stairs with [] -> false | _ -> true);
			assert (match to_stairs with [] -> false | _ -> true);
			let from_stairs_i =
				match list_index player.Being.at from_stairs with
				| Some i -> i
				| None -> Rng.Uniform.int 0 (List.length from_stairs) game.rng in
			let to_stairs_i = from_stairs_i mod List.length to_stairs in
			let player_at =
				List.nth to_stairs to_stairs_i in
			ignore (Region.transfer_being game.region player region player_at)
		end game.player;
		game.region <- region;
		game.on_level <- level_i;
		game.ai <- Ai.make region;
		game.player_info <-
			try List.assoc level_i game.saved_player_info
			with Not_found -> Player_info.make region
	end;
	(* TODO: probably don't need this? *)
	Opt.iter begin fun player ->
		update_vision game.region game.player_info player
	end game.player

let make make_level init_player check_win rng =
	let first_level_i = 0 in
	let first_level = make_level first_level_i in
	let player_at =
		let us = first_level.Region.up_stairs in
		Rng.Uniform.list_elt us rng in
	let player = init_player first_level_i first_level player_at in
	let player_info = Player_info.make first_level in
	update_vision first_level player_info player;
	{
		rng = rng;
		make_level = make_level;
		check_win = check_win;
		status = Playing;
		on_level = first_level_i;
		levels = [first_level_i, first_level];
		saved_player_info = [first_level_i, player_info];
		region = first_level;
		ai = Ai.make first_level;
		player = Some player;
		player_info = player_info;
	}

let update game player_cmd =
	let update_ai =
		match game.player with
		| None ->
			fun being ->
				Ai.update_being game.ai being, true
		| Some player ->
			fun being ->
				if being == player then begin
					player_cmd, false
				end else
					Ai.update_being game.ai being, true in
	let on_death being =
		match game.player with
		| None -> true
		| Some player ->
			if being == player then begin
				game.status <- Lost Died;
				false
			end else true in
	let on_take_stairs being dir =
		match game.player with
		| None -> true
		| Some player ->
			if being == player then begin
				set_level game (game.on_level + match dir with Up -> -1 | Down -> 1);
				false
			end else true in
	Opt.iter (Ai.update_player game.ai) game.player;
	Region.update game.region update_ai on_death on_take_stairs game.rng;
	Opt.iter begin fun player ->
		if List.exists (fun b -> b == player) game.region.Region.beings then begin
			update_vision game.region game.player_info player;
		end else
			game.player <- None
	end game.player
