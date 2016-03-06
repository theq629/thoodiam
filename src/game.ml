module Vec = Vectors.TwoD
module Map = Tilemap.Square
module Fov = Fov_adammil

type tile = char

module Thing =
	struct
		type t =
			{
				tile : tile;
			}

		let make
			~tile
			()
			=
			{ tile }
	end

module Terrain =
	struct
		type t =
			{
				tile : tile;
				blocking : bool;
			}

		let make
			~tile
			?(blocking=false)
			()
			=
			{ tile; blocking }
	end

module Cell =
	struct
		type t =
			{
				terrain : Terrain.t;
				mutable things : Thing.t list;
			}

		let make terrain =
			{
				terrain = terrain;
				things = [];
			}
	end

type dir = N | S | E | W | NE | NW | SE | SW

type command =
	| Player_move of dir
	| Quit

type t =
	{
		map : Cell.t Map.t;
		player : Thing.t;
		player_fov_radius : int;
		mutable player_at : Map.Location.t;
		mutable player_alive : bool;
		player_seen : tile option Map.t;
		player_fov : bool Map.t;
	}

let add_thing game p thing =
	let cell = Map.get game.map p in
	cell.Cell.things <- thing::cell.Cell.things

let remove_thing game p thing =
	let cell = Map.get game.map p in
	cell.Cell.things <- List.filter (fun t -> t != thing) cell.Cell.things

let dir_to_vec = function
	| N -> (0, -1)
	| S -> (0, 1)
	| E -> (1, 0)
	| W -> (-1, 0)
	| NE -> (1, -1)
	| NW -> (-1, -1)
	| SE -> (1, 1)
	| SW -> (-1, 1)

let update_vision game =
	let set_visible p =
		if Map.is_valid game.map p then begin
			Map.set game.player_fov p true;
			let cell = Map.get game.map p in
			let tile =
				match cell.Cell.things with
				| [] -> cell.Cell.terrain.Terrain.tile
				| t::_ -> t.Thing.tile in
			Map.set game.player_seen p (Some tile)
		end in
	let blocks_sight p =
		not (Map.is_valid game.map p)
		|| (Map.get game.map p).Cell.terrain.Terrain.blocking in
	Map.update game.player_fov (fun _ _ -> false);
	set_visible game.player_at;
	Fov.compute blocks_sight set_visible game.player_at game.player_fov_radius

let init map fov_radius player player_at =
	let game = {
			map = map;
			player = player;
			player_fov_radius = fov_radius;
			player_alive = true;
			player_at =	player_at;
			player_seen = Map.map map (fun _ v -> None);
			player_fov = Map.map map (fun _ _ -> false);
		} in
	add_thing game player_at player;
	update_vision game;
	game

let update game cmd =
	if game.player_alive then begin
		begin match cmd with
		| Quit ->
			game.player_alive <- false
		| Player_move dir ->
			let p1 = Vec.(game.player_at + dir_to_vec dir) in
			if (Map.is_valid game.map p1)
				&& not (Map.get game.map p1).Cell.terrain.Terrain.blocking then begin
				remove_thing game game.player_at game.player;
				add_thing game p1 game.player;
				game.player_at <- p1;
				update_vision game
			end
		end
	end
