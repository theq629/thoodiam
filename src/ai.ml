open Std
open Game_data

type t =
	{
		region : Region.t;
	}

let make region =
	{
		region = region;
	}

let update ai player =
	Opt.iter begin fun player ->
		let ai_beings = List.filter (fun b -> b != player) ai.region.Region.beings in
		List.iter begin fun being ->
			Opt.iter begin fun dir ->
				Region.queue_action ai.region being (Action.Melee_attack dir)
			end (Direction.of_vec Vec.(player.Being.at - being.Being.at))
		end ai_beings
	end player
