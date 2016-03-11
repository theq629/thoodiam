open Std
open Game_data
open Game_state
open Game_changes

module Player_info =
	struct
		type t =
			{
				seen : tile option Map.t;
				fov : bool Map.t;
			}

		let make region =
			Region.({
				seen = Map.map region.map (fun _ v -> None);
				fov = Map.map region.map (fun _ _ -> false);
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
	end Thing.(bodyable Being.(body player))

type t =
	{
		rng : Rng.Source.t;
		make_level : int -> Region.t;
		mutable on_level : int;
		mutable levels : (int * Region.t) list;
		mutable ai : Ai.t;
		mutable region : Region.t;
		mutable player : Being.t option;
		mutable player_info : Player_info.t;
	}

let set_level game level_i =
	let region =
		try
			let l = List.assoc level_i game.levels in
			Printf.eprintf "using cached level\n";
			l
		with Not_found ->
			Printf.eprintf "using new level\n";
			let new_region = game.make_level level_i in
			game.levels <- (level_i, new_region)::game.levels;
			new_region in
	Opt.iter begin fun player ->
		assert (match region.Region.down_stairs with [] -> false | _ -> true);
		assert (match region.Region.up_stairs with [] -> false | _ -> true);
		let down_stairs_i =
			match list_index player.Being.at region.Region.down_stairs with
			| Some level_i -> level_i
			| None -> Rng.Uniform.int 0 (List.length region.Region.down_stairs) game.rng in
		let player_at =
			let ds = region.Region.up_stairs in
			List.nth ds (down_stairs_i mod List.length ds) in
		ignore (Region.remove_being game.region player);
		ignore (Region.place_being region player player_at)
	end game.player;
	game.region <- region;
	game.on_level <- level_i;
	game.ai <- Ai.make region;
	game.player_info <- Player_info.make region;
	Opt.iter begin fun player ->
		update_vision game.region game.player_info player
	end game.player

let make make_level init_player rng =
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
		on_level = first_level_i;
		levels = [first_level_i, first_level];
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
		| Some player -> being != player in
	Opt.iter (Ai.update_player game.ai) game.player;
	Region.update game.region update_ai on_death game.rng;
	Opt.iter begin fun player ->
		if List.exists (fun b -> b == player) game.region.Region.beings then begin
			update_vision game.region game.player_info player;
		end else
			game.player <- None
	end game.player
