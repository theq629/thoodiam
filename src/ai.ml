open Std
open Game_data
open Game_state
open Game_changes

type floodfiller = ?neighbour_weight:(float Game_data.Map.t -> Tilemap.Location.t -> Game_data.Map.Location.t -> float -> float) -> visit:(float Game_data.Map.t -> Tilemap.Location.t -> float -> unit) -> starts:Game_data.Map.Location.t list -> unit

type t =
	{
		region : Region.t;
		mutable player_at : Map.Location.t;
		move_map : float Map.t;
		floodfiller : floodfiller;
	}

let make region =
	let move_map = Map.map region.Region.map (fun _ _ -> max_float) in
	{
		region = region;
		player_at = (-1, -1);
		move_map = move_map;
		floodfiller = Map_search.floodfill ~map:move_map;
	}

let update_move_map ai =
	let find_player_radius = 15. in
	let is_clear p =
		Region.(
			let cell = Map.get ai.region.map p in
			not cell.Cell.terrain.Terrain.blocking
		) in
	ai.floodfiller
		~neighbour_weight:begin fun _ p p1 dp ->
			if dp <= find_player_radius && is_clear p1 then Vec.(dist (float_of_int p) (float_of_int p1))
			else infinity
		end
		~visit:begin fun _ p dp ->
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
	| Some a -> a
	| None ->
		begin match try_move ai being with
		| Some a -> a
		| None -> Action.Wait
		end
