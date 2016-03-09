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

let make_stuff game rng num is_clear =
	let kinds = Thing_kinds.([|dagger; short_sword; long_sword; bastard_sword; great_sword; spear; great_sword; glaive; battle_axe; great_axe; quarterstaff; war_hammer; leather_armour; studded_leather_armour; mail_corslet; mail_hauberk|]) in
	let rand_point rng =
		let dimx, dimy = Game.Map.dim game.Game.map in
		Rng.Uniform.int 0 dimx rng, Rng.Uniform.int 0 dimy rng in
	let max_tries = num * 10 in
	let rec run t n =
		if n <= 0 || t > max_tries then ()
		else
			let kind = Rng.Uniform.array_elt kinds rng in
			let thing = Game_data.Thing.make kind in
			let p = rand_point rng in
			if is_clear game.Game.map p then begin
				Game.add_thing game p thing;
				run (t + 1) (n + 1)
			end else
				run (t + 1) n in
	run 0 num

let init map_seed things_seed =
	let map_dim = 100, 100 in
	let map_rng = Random.State.make [| map_seed |] in
	let things_rng = Random.State.make [| things_seed |] in
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
	let player = Game_data.Thing.make Thing_kinds.human in
	let is_clear map p =
		not (Game.Map.get map p).Game.Cell.terrain.Game_data.Terrain.blocking in
	Game.init map 10 player (choose_init_pos map is_clear things_rng) begin fun game ->
			make_stuff game things_rng 50 is_clear
		end
