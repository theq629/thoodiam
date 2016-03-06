module Make =
	functor (Map : Tilemap.S) ->
	struct
		type cell = Floor | Wall

		(*
		 * Cellular automata for generating caves.
		 * Simple version of http://www.roguebasin.com/index.php?title=Cellular_Automata_Method_for_Generating_Random_Cave-Like_Levels, with interface based on ROT.js implementation.
		 *)
		module Cellular =
			struct
				type rules =
					{
						born : int * int;
						survive : int * int;
					}

				let def_rules =
					{
						born = (5, max_int);
						survive = (5, max_int);
					}

				let gen =
					let update_counts map counts =
						Map.update counts begin fun p _ ->
								let c = ref 0 in
								if Map.get map p == Wall then
									incr c;
								Map.neighbours map p begin fun p1 ->
										if Map.get map p1 == Wall then
											incr c
									end;
								!c
							end in
					let update_map counts map fixed rules =
						Map.update map begin fun p v ->
							match Map.get fixed p with
							| Some v1 -> v1
							| None ->
								let c = Map.get counts p in
								let c_min, c_max = if v == Wall
									then rules.survive else rules.born in
								if c >= c_min && c <= c_max
									then Wall else Floor
						end in
					fun ?(fix=fun _ _ -> None) ?(init_wall_prob=0.45) ?(iters=[4, def_rules]) dim rng ->
						let map = Map.init dim begin fun _ ->
								if rng () <= init_wall_prob then Wall else Floor
							end in
						let fixed = Map.init dim (fix map) in
						let counts = Map.map map (fun _ _ -> 0) in
						List.iter begin fun (num_iters, rules) ->
								for i = 0 to num_iters - 1 do
									update_counts map counts;
									update_map counts map fixed rules
								done
							end iters;
					map
			end
	end
