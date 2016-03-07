module Mapgen = Mapgen.Make(Game.Map)

module Terrains =
	struct
		let floor = Game.Terrain.make
			~tile:'.'
			()

		let wall = Game.Terrain.make
			~tile:'#'
			~blocking:true
			()
	end

let choose_init_pos map is_clear rng =
	let rand_int r =
		Random.State.int rng (2 * r) - r in
	let dim = Game.Map.dim map in
	let centre = Game.Vec.(dim / 2) in
	let rec run radius =
		let p = Game.Vec.(centre + (rand_int radius, rand_int radius)) in
		if is_clear map p then p
		else run (radius + 1) in
	run 1

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
	let player_kind = Game.Thing.Kind.make
		~tile:'@'
		~name:"A player"
		weight:100.
		() in
	let player = Game.Thing.make player_kind in
	let is_clear map p =
		not (Game.Map.get map p).Game.Cell.terrain.Game.Terrain.blocking in
	Game.init map 10 player (choose_init_pos map is_clear things_rng)
