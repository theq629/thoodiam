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

type t =
	{
		rng : Rng.Source.t;
		region : Region.t;
		ai : Ai.t;
		mutable player : Being.t option;
		player_info : Player_info.t;
	}

let make region rng =
	{
		rng = rng;
		region = region;
		ai = Ai.make region;
		player = None;
		player_info = Player_info.make region;
	}

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

let set_player game player =
	update_vision game.region game.player_info player;
	game.player <- Some player

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
