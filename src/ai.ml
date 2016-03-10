open Std
open Game_data
module Map_search = Graph_search.Make(Tilemap.Location)

type t =
	{
		region : Region.t;
		mutable player_at : Map.Location.t;
		move_map : float Map.t;
	}

let make region =
	{
		region = region;
		player_at = (-1, -1);
		move_map = Map.map region.Region.map (fun _ _ -> max_float);
	}

let update_move_map ai =
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
		~starts:[ai.player_at]

let try_attack ai being =
	let attack_dir = Direction.of_vec Vec.(ai.player_at - being.Being.at) in
	match attack_dir with
	| None -> None
	| Some dir -> Some (Action.Melee_attack dir)

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
		| None -> None
		| Some dir -> Some (Action.Move dir)
	end else None

let update_player ai player =
	ai.player_at <- player.Being.at;
	update_move_map ai

let update_being ai being =
	match try_attack ai being with
	| Some a -> Some a
	| None -> try_move ai being
