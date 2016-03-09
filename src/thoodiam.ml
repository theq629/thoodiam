module Mapgen = Mapgen.Make(Game.Map)
module Rng = Game_data.Rng
open Thoodiam_data

let choose_init_pos map is_clear rng =
	let rand_int r =
		Rng.Uniform.int (-r) r rng in
	let dim = Game.Map.dim map in
	let centre = Game.Vec.(dim / 2) in
	let rec run radius =
		let p = Game.Vec.(centre + (rand_int radius, rand_int radius)) in
		if is_clear map p then p
		else run (radius + 1) in
	run 1

let for_clear_points ?max_tries map is_clear num rng f =
	let max_tries =
		match max_tries with
		| Some n -> n
		| None -> num * 10 in
	let rand_point rng =
		let dimx, dimy = Game.Map.dim map in
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

let make_stuff game rng num is_clear =
	let kinds = Thing_kinds.([|dagger; short_sword; long_sword; bastard_sword; great_sword; spear; great_sword; glaive; battle_axe; great_axe; quarterstaff; war_hammer; leather_armour; studded_leather_armour; mail_corslet; mail_hauberk|]) in
	for_clear_points game.Game.map is_clear num rng begin fun p ->
		let kind = Rng.Uniform.array_elt kinds rng in
		let thing = Game_data.Thing.make kind in
		Game.add_thing game p thing
	end

let make_creatures game rng num is_clear =
	let kinds = Thing_kinds.([|
			goblin, Game_data.Being.({ melee = 8; evasion = 6 });
			orc, Game_data.Being.({ melee = 6; evasion = 8 })
		|]) in
	for_clear_points game.Game.map is_clear num rng begin fun p ->
		let kind, skills = Rng.Uniform.array_elt kinds rng in
		Game.init_being game kind  skills p
	end

let init map_seed things_seed game_seed =
	let map_dimx, map_dimy as map_dim = 100, 100 in
	let map_area = map_dimx * map_dimy in
	let map_rng = Random.State.make [| map_seed |] in
	let things_rng = Random.State.make [| things_seed |] in
	let game_rng = Random.State.make [| game_seed |] in
	let raw_map = Mapgen.Cellular.gen
		~fix:(fun m p -> if Game.Map.is_boundary m p then Some Mapgen.Wall else None)
		map_dim
		(fun () -> Random.State.float map_rng 1.) in
	let map = Game.Map.map raw_map begin fun _ value ->
			let terrain =
				match value with
				| Mapgen.Floor -> Terrains.floor
				| Mapgen.Wall -> Terrains.wall in
			Game.Cell.make terrain
		end in
	let is_clear map p =
		Game.(Game_data.(
			let cell = Map.get map p in
			not cell.Cell.terrain.Terrain.blocking
			&& not (List.exists (fun t -> t.Thing.kind.Thing.Kind.visual_priority) cell.Cell.things)
		)) in
	let player_skills =
		Game_data.Being.({
			melee = 10;
			evasion = 7;
		}) in
	Game.init map 10 begin fun game ->
			make_stuff game things_rng (map_area / 500) is_clear;
			make_creatures game things_rng (map_area / 500) is_clear;
			let player = Game.init_being game Thing_kinds.human player_skills (choose_init_pos map is_clear things_rng) in
			Game.set_player game player
		end game_rng
