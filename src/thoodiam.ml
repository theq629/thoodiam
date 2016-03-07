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

module Thing_kinds =
	struct
		open Game
		open Thing.Kind

		let dagger =
			make
				~tile:'/'
				~name:"Dagger"
				~weight:2.
				~melee:Combat.(make
						~accuracy:0
						~damage:(Dice.make 1 5)
					())
				()

		let short_sword =
			make
				~tile:'/'
				~name:"Short sword"
				~weight:5.
				~melee:Combat.(make
						~accuracy:0
						~damage:(Dice.make 0 7)
						~evasion:1
					())
				()

		let long_sword =
			make
				~tile:'/'
				~name:"Longsword"
				~weight:10.
				~melee:Combat.(make
						~accuracy:0
						~damage:(Dice.make 2 5)
						~evasion:1
					())
				()

		let bastard_sword =
			make
				~tile:'/'
				~name:"Bastard sword"
				~weight:15.
				~melee:Combat.(make
						~accuracy:(-1)
						~damage:(Dice.make 3 3)
						~evasion:1
					())
				()

		let great_sword =
			make
				~tile:'/'
				~name:"Greatsword"
				~weight:20.
				~melee:Combat.(make
						~accuracy:(-1)
						~damage:(Dice.make 3 5)
						~evasion:1
					())
				()

		let spear =
			make
				~tile:'/'
				~name:"Spear"
				~weight:15.
				~melee:Combat.(make
						~accuracy:(-1)
						~damage:(Dice.make 1 9)
						~evasion:0
					())
				()

		let great_spear =
			make
				~tile:'/'
				~name:"Great Spear"
				~weight:15.
				~melee:Combat.(make
						~accuracy:(-1)
						~damage:(Dice.make 1 13)
						~evasion:1
					())
				()

		let glaive =
			make
				~tile:'/'
				~name:"Glaive"
				~weight:20.
				~melee:Combat.(make
						~accuracy:(-2)
						~damage:(Dice.make 2 9)
						~evasion:1
					())
				()

		let battle_axe =
			make
				~tile:'/'
				~name:"Battle Axe"
				~weight:10.
				~melee:Combat.(make
						~accuracy:(-3)
						~damage:(Dice.make 3 4)
						~evasion:0
					())
				()

		let great_axe =
			make
				~tile:'/'
				~name:"Battle Axe"
				~weight:15.
				~melee:Combat.(make
						~accuracy:(-4)
						~damage:(Dice.make 4 4)
						~evasion:0
					())
				()

		let quarterstaff =
			make
				~tile:'/'
				~name:"Quarterstaff"
				~weight:3.
				~melee:Combat.(make
						~accuracy:0
						~damage:(Dice.make 2 3)
						~evasion:2
					())
				()

		let war_hammer =
			make
				~tile:'/'
				~name:"War Hammer"
				~weight:10.
				~melee:Combat.(make
						~accuracy:(-2)
						~damage:(Dice.make 4 1)
						~evasion:0
					())
				()

		let human =
			make
				~tile:'@'
				~name:"Human"
				~weight:100.
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

let make_stuff game rng num is_clear =
	let kinds = Thing_kinds.([|dagger; short_sword; long_sword; bastard_sword; great_sword; spear; great_sword; glaive; battle_axe; great_axe; quarterstaff; war_hammer|]) in
	let rand_point rng =
		let dimx, dimy = Game.Map.dim game.Game.map in
		Random.State.int rng dimx, Random.State.int rng dimy in
	let max_tries = num * 10 in
	let rec run t n =
		if n <= 0 || t > max_tries then ()
		else
			let i = Random.State.int rng (Array.length kinds) in

			let thing = Game.Thing.make kinds.(i) in
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
	let player = Game.Thing.make Thing_kinds.human in
	let is_clear map p =
		not (Game.Map.get map p).Game.Cell.terrain.Game.Terrain.blocking in
	Game.init map 10 player (choose_init_pos map is_clear things_rng) begin fun game ->
			make_stuff game things_rng 50 is_clear
		end
