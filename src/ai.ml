open Std
open Game_data
module Map_search = Graph_search.Make(Tilemap.Location)

type t =
	{
		region : Region.t;
		move_map : float Map.t;
	}

let make region =
	{
		region = region;
		move_map = Map.map region.Region.map (fun _ _ -> max_float);
	}

let update_move_map ai player =
	let is_clear p =
		Region.(
			let cell = Map.get ai.region.map p in
			not cell.Cell.terrain.Terrain.blocking
		) in
	Map_search.floodfill
		~neighbours:begin fun p dp f ->
			if dp <= 20. then
				Map.neighbours ai.region.Region.map p begin fun p1 ->
					if is_clear p1 then
						f p1 Vec.(dist (float_of_int p) (float_of_int p1))
				end
		end
		~visit:begin fun p dp ->
			Map.set ai.move_map p dp
		end
		~starts:[player.Being.at]

let try_attack ai being player =
	let attack_dir = Direction.of_vec Vec.(player.Being.at - being.Being.at) in
	match attack_dir with
	| None -> false
	| Some dir -> 
		Region.queue_action ai.region being (Action.Melee_attack dir);
		true

let try_move ai being =
	let d_at = Map.get ai.move_map being.Being.at in
	let min_p = ref (-1, -1) in
	let min_d = ref max_float in
	Map.neighbours ai.move_map being.Being.at begin fun p1 ->
		let dp1 = Map.get ai.move_map p1 in
		if dp1 < !min_d then begin
			min_p := p1;	
			min_d := dp1
		end
	end;
	if !min_d < d_at then begin
		let move_dir = Direction.of_vec Vec.(!min_p - being.Being.at) in
		match move_dir with
		| None -> false
		| Some dir ->
			Region.queue_action ai.region being (Action.Move dir);
			true
	end else false

let update ai player =
	Opt.iter begin fun player ->
		update_move_map ai player;
		let ai_beings = List.filter (fun b -> b != player) ai.region.Region.beings in
		List.iter begin fun being ->
			ignore (
				try_attack ai being player
				|| try_move ai being
			)
		end ai_beings
	end player
