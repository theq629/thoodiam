module Vec = Vectors.TwoD
module Map = Tilemap.Square
module Fov = Fov_adammil
open Game_data

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

module Message =
	struct
		type t =
			| Pick_up of (Being.t * Thing.t)
			| Drop of (Being.t * Thing.t)
			| Equip of (Being.t * Equip_slot.t * Thing.t)
			| Unequip of (Being.t * Equip_slot.t * Thing.t)
			| Die of Being.t
	end

type dir = N | S | E | W | NE | NW | SE | SW

type command =
	| Move of dir
	| Pick_up of Thing.t
	| Drop of Thing.t
	| Equip of (Thing.t * Equip_slot.t)
	| Unequip of Equip_slot.t
	| Quit

type t =
	{
		map : Cell.t Map.t;
		mutable player : Being.t option;
		player_seen : tile option Map.t;
		player_fov : bool Map.t;
		mutable messages : Message.t list;
	}

let dir_to_vec = function
	| N -> (0, -1)
	| S -> (0, 1)
	| E -> (1, 0)
	| W -> (-1, 0)
	| NE -> (1, -1)
	| NW -> (-1, -1)
	| SE -> (1, 1)
	| SW -> (-1, 1)

let remove_from_list xs x =
	let found = ref false in
	let xs1 = List.filter begin fun y ->
			if x = y then begin
				found := true;
				false
			end else
				true
		end xs in
	(* TODO: faster? *)
	!found, xs1

let add_thing game p thing =
	let cell = Map.get game.map p in
	cell.Cell.things <- thing::cell.Cell.things

let remove_thing game p thing =
	let cell = Map.get game.map p in
	let found, things1 = remove_from_list cell.Cell.things thing in
	cell.Cell.things <- things1;
	found

let remove_being game being =
	remove_thing game being.Being.at being.Being.body

let place_being game being at =
	let found = remove_thing game being.Being.at being.Being.body in
	add_thing game at being.Being.body;
	being.Being.at <- at;
	found

let add_msg game at msg =
	(* TODO: check if player can see location *)
	game.messages <- msg::game.messages

let init_being ?(vison_radius=10) game body at =
	let being = Being.({
			vison_radius = vison_radius;
			at = at;
			body = body;
			inv = [];
			equip = [];
		}) in
	ignore (place_being game being being.Being.at);
	being

let update_vision game player =
	let set_visible p =
		if Map.is_valid game.map p then begin
			Map.set game.player_fov p true;
			let cell = Map.get game.map p in
			let tile =
				let rec run =
					function
					| [] -> cell.Cell.terrain.Terrain.tile
					| [t] -> Thing.(Kind.(t.kind.tile))
					| t::_ when Thing.(Kind.(t.kind.visual_priority)) -> Thing.(Kind.(t.kind.tile))
					| _::ts1 -> run ts1 in
				run cell.Cell.things in
			Map.set game.player_seen p (Some tile)
		end in
	let blocks_sight p =
		not (Map.is_valid game.map p)
		|| (Map.get game.map p).Cell.terrain.Terrain.blocking in
	Map.update game.player_fov (fun _ _ -> false);
	set_visible player.Being.at;
	Fov.compute blocks_sight set_visible player.Being.at player.Being.vison_radius

let init map fov_radius player_body player_at configure =
	let game = {
			map = map;
			player = None;
			player_seen = Map.map map (fun _ v -> None);
			player_fov = Map.map map (fun _ _ -> false);
			messages = [];
		} in
	configure game;
	let player = init_being game player_body player_at in
	game.player <- Some player;
	update_vision game player;
	game

let update_player game player cmd =
	match cmd with
	| Quit -> begin
			add_msg game player.Being.at (Message.Die player);
			ignore (remove_being game player);
			game.player <- None
		end
	| Move dir -> begin
			let p1 = Vec.(player.Being.at + dir_to_vec dir) in
			if (Map.is_valid game.map p1)
				&& not (Map.get game.map p1).Cell.terrain.Terrain.blocking then begin
				ignore (place_being game player p1);
				update_vision game player
			end
		end
	| Pick_up thing -> begin
			let found = remove_thing game player.Being.at thing in
			if found then begin
				add_msg game player.Being.at (Message.Pick_up (player, thing));
				player.Being.inv <- thing::player.Being.inv
			end
		end
	| Drop thing -> begin
			let found, inv1 = remove_from_list player.Being.inv thing in
			if found then begin
				add_msg game player.Being.at (Message.Drop (player, thing));
				player.Being.inv <- inv1;
				add_thing game player.Being.at thing
			end
		end
	| Equip (thing, equip_slot) -> begin
			if List.mem equip_slot player.Being.body.Thing.kind.Thing.Kind.equip_slots && not (List.mem_assoc equip_slot player.Being.equip) then begin
				let found, inv1 = remove_from_list player.Being.inv thing in
				if found then begin
					add_msg game player.Being.at (Message.Equip (player, equip_slot, thing));
					player.Being.inv <- inv1;
					player.Being.equip <- (equip_slot, thing)::player.Being.equip
				end
			end
		end
	| Unequip equip_slot -> begin
			try
				let thing = List.assoc equip_slot player.Being.equip in
				add_msg game player.Being.at (Message.Unequip (player, equip_slot, thing));
				player.Being.equip <- List.remove_assoc equip_slot player.Being.equip;
				player.Being.inv <- thing :: player.Being.inv
			with Not_found -> ()
		end

let update game cmds =
	game.messages <- [];
	begin match game.player with
	| Some p ->
		List.iter (fun c -> update_player game p c) cmds
	| None -> ()
	end
