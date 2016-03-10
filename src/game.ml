open Std
open Game_data

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
					| [t] -> Thing.(Kind.(t.kind.tile))
					| t::_ when Thing.(Kind.(t.kind.visual_priority)) -> Thing.(Kind.(t.kind.tile))
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
	end player.Being.body.Thing.kind.Thing.Kind.bodyable

let set_player game player =
	update_vision game.region game.player_info player;
	game.player <- Some player

let update game player_cmds =
	Ai.update game.ai game.player;
	Opt.iter begin fun player ->
		List.iter begin fun cmd ->
			Region.queue_action game.region player cmd
		end player_cmds;
	end game.player;
	Region.update game.region game.rng;
	Opt.iter begin fun player ->
		if List.exists (fun b -> b == player) game.region.Region.beings then begin
			update_vision game.region game.player_info player
		end else
			game.player <- None
	end game.player
