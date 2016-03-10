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

let rand_weapon_kinds = Thing_kinds.([|dagger; short_sword; long_sword; bastard_sword; great_sword; spear; great_sword; glaive; battle_axe; great_axe; quarterstaff; war_hammer|])
let rand_armour_kinds = Thing_kinds.([|leather_armour; studded_leather_armour; mail_corslet; mail_hauberk|])
let rand_thing_kinds = Array.append rand_weapon_kinds rand_armour_kinds

let make_stuff region rng num is_clear =
	for_clear_points region.Region.map is_clear num rng begin fun p ->
		let kind = Rng.Uniform.array_elt rand_thing_kinds rng in
		let thing = Thing.make kind in
		Region.add_thing region p thing
	end

let equip_being being rng =
	let weapon = Thing.make (Rng.Uniform.array_elt rand_weapon_kinds rng) in
	let armour = Thing.make (Rng.Uniform.array_elt rand_armour_kinds rng) in
	being.Being.equip <- [
			Equip_slots.melee_weapon, weapon;
			Equip_slots.armour, armour
		]

let make_creatures region rng num is_clear =
	let kinds = Thing_kinds.([|
			goblin, Being.({ melee = 8; evasion = 6 });
			orc, Being.({ melee = 6; evasion = 8 })
		|]) in
	for_clear_points region.Region.map is_clear num rng begin fun p ->
		let kind, skills = Rng.Uniform.array_elt kinds rng in
		let being = Region.init_being region kind skills p in
		equip_being being rng
	end

let is_clear map p =
	Region.(
		let cell = Map.get map p in
		not cell.Cell.terrain.Terrain.blocking
		&& not (List.exists (fun t -> t.Thing.kind.Thing.Kind.visual_priority) cell.Cell.things)
	)

let make_region map_rng things_rng =
	let map_dimx, map_dimy as map_dim = 100, 100 in
	let map_area = map_dimx * map_dimy in
	let raw_map = Mapgen.Cellular.gen
		~fix:(fun m p -> if Map.is_boundary m p then Some Mapgen.Wall else None)
		map_dim
		(fun () -> Random.State.float map_rng 1.) in
	let map = Map.map raw_map begin fun _ value ->
			let terrain =
				match value with
				| Mapgen.Floor -> Terrains.floor
				| Mapgen.Wall -> Terrains.wall in
			Region.Cell.make terrain
		end in
	Region.init map begin fun region ->
		make_stuff region things_rng (map_area / 500) is_clear;
		make_creatures region things_rng (map_area / 500) is_clear
	end

let init map_seed things_seed game_seed =
	let map_rng = Random.State.make [| map_seed |] in
	let things_rng = Random.State.make [| things_seed |] in
	let game_rng = Random.State.make [| game_seed |] in
	let player_skills =
		Game_data.Being.({
			melee = 10;
			evasion = 7;
		}) in
	let region = make_region map_rng things_rng in
	let game = Game.make region game_rng in
	let player = Region.init_being region Thing_kinds.human player_skills (choose_init_pos region.Region.map is_clear things_rng) in
	equip_being player things_rng;
	Game.set_player game player;
	game
