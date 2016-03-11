module Mapgen = Mapgen.Make(Game_data.Map)
module Rng = Game_data.Rng
open Game_data
open Game_state
open Game_changes
open Thoodiam_data

let for_clear_points ?max_tries map is_clear num rng f =
	let rand_point rng =
		let dimx, dimy = Map.dim map in
		Rng.Uniform.int 0 dimx rng, Rng.Uniform.int 0 dimy rng in
	let max_tries =
		match max_tries with
		| Some n -> n
		| None -> num * 10 in
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

let choose_stair_points ~max_tries map is_clear num_ups num_downs min_stair_dist rng =
	let make_points is_clear num =
		let points = ref [] in
		for_clear_points ~max_tries:10 map is_clear num rng begin fun p ->
			points := p::!points
		end;
		!points in
	let check_pathing finishes =
		let module Point_set = Set.Make(struct type t = int * int;; let compare = compare;; end) in
		let finishes = Point_set.of_list finishes in
		fun start ->
			let path = Map_search.dijkstra
				~neighbours:begin fun p dp f ->
					Map.neighbours map p begin fun p1 ->
						if is_clear map p1 then
							f p1 Vec.(dist (float_of_int p) (float_of_int p1))
					end
				end
				~visit:begin fun p dp ->
					dp >= min_stair_dist && Point_set.mem p finishes
				end
				~starts:[start] in
			match path with
			| None -> false
			| Some _ -> true in
	Retry.retry ~max_tries begin fun () ->
		let downs = make_points is_clear num_downs in
		let has_path_to_down = check_pathing downs in
		let ups = make_points (fun map p -> is_clear map p && has_path_to_down p) num_ups in
		match ups, downs with
		| _::_, _::_ -> Retry.Ok (ups, downs)
		| _ -> Retry.Failed
	end

let rand_from_array = Rng.Empirical.array_elt ~weight:fst ~value:snd

let make_stuff region num is_clear use_kinds rng =
	let rand_kind = rand_from_array use_kinds in
	for_clear_points region.Region.map is_clear num rng begin fun p ->
		let kind = rand_kind rng in
		let thing = Thing.make kind in
		Region.add_thing region p thing
	end

let equip_being ?(max_tries=10) use_weapon_kinds use_armour_kinds =
	let rand_weapon = rand_from_array use_weapon_kinds in
	let rand_armour = rand_from_array use_armour_kinds in
	fun being rng ->
		let rec run num_tries =
			if num_tries >= max_tries then ()
			else begin
				let weapon = Thing.(make (rand_weapon rng)) in
				let armour = Thing.(make (rand_armour rng)) in
					let a = Being.(get being weapon) in
					let b = Being.(get being armour) in
					let c = Being.(equip being Equip_slots.melee_weapon weapon) in
					let d = Being.(equip being Equip_slots.armour armour) in
				let ok =
					a && b && c && d in
				if ok then ()
				else begin
					ignore (Being.unequip being Equip_slots.melee_weapon);
					ignore (Being.unequip being Equip_slots.armour);
					ignore (Being.lose being weapon);
					ignore (Being.lose being armour);
					run (num_tries + 1)
				end
			end in
		run 0

let make_creatures region num is_clear use_being_kind use_weapon_kinds use_armour_kinds rng =
	let rand_kind = rand_from_array use_being_kind in
	let equip = equip_being use_weapon_kinds use_armour_kinds in
	for_clear_points region.Region.map is_clear num rng begin fun p ->
		let kind = rand_kind rng in
		let being = Region.init_being region kind p in
		equip being rng
	end

let make_player region at being_kind use_weapon_kinds use_armour_kinds rng =
	let player = Region.init_being region being_kind at in
	equip_being ~max_tries:100 use_weapon_kinds use_armour_kinds player rng;
	player

let is_clear map p =
	Region.(
		let cell = Map.get map p in
		not cell.Cell.terrain.Terrain.blocking
		&& not (List.exists Thing.blocks cell.Cell.things)
	)

let make_map ~max_tries dim num_stairs min_stair_dist has_downs rng =
	Retry.retry ~max_tries begin fun () ->
		let raw_map = Mapgen.Cellular.(gen
				~fix:(fun m p -> if Map.is_boundary m p then Some Mapgen.Wall else None)
				~iters:[3, { born = (5, 8); survive = (5, 8) }]
				dim
				(fun () -> Random.State.float rng 1.)
			) in
		let map = Map.map raw_map begin fun _ value ->
				let terrain =
					match value with
					| Mapgen.Floor -> Terrains.floor
					| Mapgen.Wall -> Terrains.wall in
				Region.Cell.make terrain
			end in
		Retry.map begin fun (ups, downs) ->
			List.iter begin fun p ->
				(Map.get map p).Region.Cell.terrain <- Terrains.stairs_up
			end ups;
			let use_downs =
				if has_downs then begin
					List.iter begin fun p ->
						(Map.get map p).Region.Cell.terrain <- Terrains.stairs_down
					end downs;
					downs
				end else [] in
			(map, ups, use_downs)
		end (choose_stair_points ~max_tries:10 map is_clear num_stairs num_stairs min_stair_dist rng)
	end

let make_region spec map_rng things_rng =
	let map_dimx, map_dimy as map_dim = 100, 100 in
	let map_area = map_dimx * map_dimy in
	let num_stairs = map_area / 1000 in
	let min_stair_dist = float_of_int (max map_dimx map_dimy) /. 4. in
	let map, ups, downs =
		match make_map ~max_tries:max_int map_dim num_stairs min_stair_dist spec.Level_spec.has_down_stairs map_rng with
		| Retry.Ok m -> m
		| _ -> assert false in
	Level_spec.(
		let all_things = Array.append spec.weapon_kinds spec.armour_kinds in
		let region =
			Region.init map ups downs begin fun region ->
				make_stuff region (map_area / 2000) is_clear all_things things_rng;
				make_creatures region (map_area / 2000) is_clear spec.enemy_kinds spec.weapon_kinds spec.armour_kinds things_rng
			end in
		region
	)

let init map_seed things_seed game_seed =
	let map_rng = Random.State.make [| map_seed |] in
	let things_rng = Random.State.make [| things_seed |] in
	let game_rng = Random.State.make [| game_seed |] in
	let make_level level_i =
		let spec = level_specs.(level_i) in
		make_region spec map_rng things_rng in
	let init_player level_i region player_at =
		let spec = level_specs.(level_i) in
		Level_spec.(
			make_player region player_at Thing_kinds.human spec.weapon_kinds spec.armour_kinds things_rng
		) in
	let game = Game.make make_level init_player game_rng in
	game
