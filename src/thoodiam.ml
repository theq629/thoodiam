module Mapgen = Mapgen.Make(Game_data.Map)
module Rng = Game_data.Rng
open Game_data
open Thoodiam_data

let choose_init_pos map is_clear rng =
	let rand_int r =
		Rng.Uniform.int (-r) r rng in
	let dim = Map.dim map in
	let centre = Vec.(dim / 2) in
	let rec run radius =
		let p = Vec.(centre + (rand_int radius, rand_int radius)) in
		if is_clear map p then p
		else run (radius + 1) in
	run 1

let for_clear_points ?max_tries map is_clear num rng f =
	let max_tries =
		match max_tries with
		| Some n -> n
		| None -> num * 10 in
	let rand_point rng =
		let dimx, dimy = Map.dim map in
		Rng.Uniform.int 0 dimx rng, Rng.Uniform.int 0 dimy rng in
	let rec run t n =
		if n <= 0 || t > max_tries then ()
		else
			let p = rand_point rng in
			if is_clear map p then begin
				f p;
				run (t + 1) (n + 1)
			end else
				run (t + 1) n in
	run 0 num

let rand_from_array = Rng.Empirical.array_elt ~weight:fst ~value:snd

let make_stuff region num is_clear use_kinds rng =
	let rand_kind = rand_from_array use_kinds in
	for_clear_points region.Region.map is_clear num rng begin fun p ->
		let kind = rand_kind rng in
		let thing = Thing.make kind in
		Region.add_thing region p thing
	end

let make_creatures region num is_clear use_being_kind use_weapon_kinds use_armour_kinds rng =
	let rand_kind = rand_from_array use_being_kind in
	let rand_weapon = rand_from_array use_weapon_kinds in
	let rand_armour = rand_from_array use_armour_kinds in
	for_clear_points region.Region.map is_clear num rng begin fun p ->
		let kind = rand_kind rng in
		let being = Region.init_being region kind p in
		being.Being.equip <- [
				Equip_slots.melee_weapon, Thing.make (rand_weapon rng);
				Equip_slots.armour, Thing.make (rand_armour rng)
			]
	end

let make_player region is_clear being_kind use_weapon_kinds use_armour_kinds rng =
	let rand_weapon = rand_from_array use_weapon_kinds in
	let rand_armour = rand_from_array use_armour_kinds in
	let at = choose_init_pos region.Region.map is_clear rng in
	let player = Region.init_being region being_kind at in
	player.Being.equip <- [
			Equip_slots.melee_weapon, Thing.make (rand_weapon rng);
			Equip_slots.armour, Thing.make (rand_armour rng)
		];
	player

let is_clear map p =
	Region.(
		let cell = Map.get map p in
		not cell.Cell.terrain.Terrain.blocking
		&& not (List.exists (fun t -> t.Thing.kind.Thing.Kind.visual_priority) cell.Cell.things)
	)

let make_region spec map_rng things_rng =
	let map_dimx, map_dimy as map_dim = 100, 100 in
	let map_area = map_dimx * map_dimy in
	let raw_map = Mapgen.Cellular.(gen
			~fix:(fun m p -> if Map.is_boundary m p then Some Mapgen.Wall else None)
			~iters:[3, { born = (5, 8); survive = (5, 8) }]
			map_dim
			(fun () -> Random.State.float map_rng 1.)
		) in
	let map = Map.map raw_map begin fun _ value ->
			let terrain =
				match value with
				| Mapgen.Floor -> Terrains.floor
				| Mapgen.Wall -> Terrains.wall in
			Region.Cell.make terrain
		end in
	Level_spec.(
		let all_things = Array.append spec.weapon_kinds spec.armour_kinds in
		Region.init map begin fun region ->
			make_stuff region (map_area / 2000) is_clear all_things things_rng;
			make_creatures region (map_area / 2000) is_clear spec.enemy_kinds spec.weapon_kinds spec.armour_kinds things_rng
		end
	)

let init map_seed things_seed game_seed =
	let map_rng = Random.State.make [| map_seed |] in
	let things_rng = Random.State.make [| things_seed |] in
	let game_rng = Random.State.make [| game_seed |] in
	let level_spec = level_specs.(0) in
	let region = make_region level_spec map_rng things_rng in
	let game = Game.make region game_rng in
	let player = make_player region is_clear Thing_kinds.human level_spec.Level_spec.weapon_kinds level_spec.Level_spec.armour_kinds things_rng in
	Game.set_player game player;
	game
